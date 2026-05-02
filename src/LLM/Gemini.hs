module LLM.Gemini (geminiClient, parseResponse, parseUsage) where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Unique (hashUnique, newUnique)
import LLM.SSE
import LLM.Types
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
import Network.HTTP.Types.Status (statusCode)

geminiClient :: Text -> LLMClient
geminiClient apiKey =
  LLMClient
    { clientChat = geminiChat apiKey,
      clientChatStream = Just (geminiChatStream apiKey)
    }

geminiChat :: Text -> ChatRequest -> IO LLMResult
geminiChat apiKey r = do
  result <- try $ runReq lenientConfig $ do
    let url =
          https "generativelanguage.googleapis.com"
            /: "v1beta"
            /: "models"
            /: (reqModel r <> ":generateContent")
    resp <- req POST url (ReqBodyJson (buildBody r)) jsonResponse ("key" =: apiKey)
    let status = responseStatusCode resp
        body = responseBody resp :: Value
    pure $
      if status == 200
        then Right body
        else Left $ HttpError status (T.pack $ show body)
  case result of
    Left e -> pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right (Left err) -> pure $ Left err
    Right (Right body) -> parseResponse body

geminiChatStream :: Text -> ChatRequest -> (StreamEvent -> IO ()) -> IO LLMResult
geminiChatStream apiKey r callback = do
  result <- try $ runReq lenientConfig $ do
    let url =
          https "generativelanguage.googleapis.com"
            /: "v1beta"
            /: "models"
            /: (reqModel r <> ":streamGenerateContent")
    reqBr POST url (ReqBodyJson (buildBody r)) ("key" =: apiKey <> "alt" =: ("sse" :: Text)) $ \resp -> do
      let status = statusCode (HC.responseStatus resp)
      if status /= 200
        then do
          chunks <- readAll (HC.responseBody resp)
          pure $ Left $ HttpError status (decodeUtf8 (BS.concat chunks))
        else parseGeminiStream (HC.responseBody resp) callback
  case result of
    Left e -> pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right r' -> pure r'

readAll :: HC.BodyReader -> IO [BS.ByteString]
readAll br = do
  chunk <- HC.brRead br
  if BS.null chunk then pure [] else (chunk :) <$> readAll br

parseGeminiStream :: HC.BodyReader -> (StreamEvent -> IO ()) -> IO LLMResult
parseGeminiStream reader callback = do
  blocksRef <- newIORef ([] :: [ContentBlock])
  usageRef <- newIORef Nothing
  readSSEEvents (HC.brRead reader) $ \sse -> do
    case decodeStrict' (encodeUtf8 (sseData sse)) of
      Nothing -> pure ()
      Just v -> do
        -- Each SSE chunk is a complete response fragment; parse parts from it
        case parseMaybe parseChunkParts v of
          Just parts -> do
            newBlocks <- mapM (assignToolId callback) parts
            modifyIORef' blocksRef (++ newBlocks)
          Nothing -> pure ()
        -- Check for usage metadata (usually in the last chunk)
        case parseMaybe parseUsageMetadata v of
          Just u -> writeIORef usageRef (Just u)
          Nothing -> pure ()
  blocks <- readIORef blocksRef
  usage <- readIORef usageRef
  let text = T.concat [t | TextBlock t <- blocks]
  if null blocks
    then pure $ Left EmptyResponse
    else pure $ Right (ChatResponse text blocks usage)
  where
    assignToolId :: (StreamEvent -> IO ()) -> ContentBlock -> IO ContentBlock
    assignToolId cb (TextBlock t) = do
      cb (StreamDelta t)
      pure (TextBlock t)
    assignToolId cb (ToolCallBlock tc) = do
      tc' <- normalizeToolCallId tc
      cb (StreamToolCall tc')
      pure (ToolCallBlock tc')

    parseChunkParts :: Value -> Parser [ContentBlock]
    parseChunkParts = withObject "GeminiChunk" $ \o -> do
      (cand : _) <- o .: "candidates" :: Parser [Value]
      withObject
        "candidate"
        ( \co -> do
            cont <- co .: "content"
            withObject "content" (\cco -> cco .: "parts" >>= mapM parsePartBlock) cont
        )
        cand

    parsePartBlock :: Value -> Parser ContentBlock
    parsePartBlock = withObject "part" $ \o -> do
      let tryText = TextBlock <$> (o .: "text")
          tryFunctionCall = do
            fc <- o .: "functionCall"
            withObject
              "functionCall"
              ( \fco -> do
                  name <- fco .: "name"
                  args <- fco .:? "args" .!= object []
                  pure $ ToolCallBlock (ToolCall name name args)
              )
              fc
      tryText <|> tryFunctionCall

    parseUsageMetadata :: Value -> Parser Usage
    parseUsageMetadata = withObject "GeminiChunk" $ \o -> do
      u <- o .: "usageMetadata"
      withObject
        "usageMetadata"
        (\uo -> Usage <$> uo .: "promptTokenCount" <*> uo .: "candidatesTokenCount")
        u

-- Don't let req throw on non-2xx; we handle it ourselves
lenientConfig :: HttpConfig
lenientConfig =
  defaultHttpConfig
    { httpConfigCheckResponse = \_ _ _ -> Nothing
    }

buildBody :: ChatRequest -> Value
buildBody r =
  object $
    [ "contents" .= concatMap encodeTurn (reqConversation r),
      "generationConfig" .= genConfig r
    ]
      ++ [ "system_instruction" .= object ["parts" .= [object ["text" .= sys]]]
           | Just sys <- [reqSystem r]
         ]
      ++ [ "tools" .= [object ["function_declarations" .= map encodeToolDef (reqTools r)]]
           | not (null (reqTools r))
         ]

encodeTurn :: Turn -> [Value]
encodeTurn (UserTurn content) =
  [ object
      [ "role" .= ("user" :: Text),
        "parts" .= [object ["text" .= content]]
      ]
  ]
encodeTurn (AssistantTurn text calls) =
  [ object
      [ "role" .= ("model" :: Text),
        "parts" .= (textParts ++ callParts)
      ]
  ]
  where
    textParts = [object ["text" .= text] | not (T.null text)]
    callParts = map encodeFunctionCall calls
encodeTurn (ToolTurn results) =
  [ object
      [ "role" .= ("user" :: Text),
        "parts" .= map encodeFunctionResponse results
      ]
  ]

encodeToolDef :: ToolDef -> Value
encodeToolDef td =
  object
    [ "name" .= toolName td,
      "description" .= toolDescription td,
      "parameters" .= toolParameters td
    ]

encodeFunctionCall :: ToolCall -> Value
encodeFunctionCall tc =
  object
    [ "functionCall"
        .= object
          [ "name" .= tcName tc,
            "args" .= tcArguments tc
          ]
    ]

encodeFunctionResponse :: ToolResult -> Value
encodeFunctionResponse tr =
  object
    [ "functionResponse"
        .= object
          [ "name" .= trName tr,
            "response" .= object ["result" .= trContent tr]
          ]
    ]

-- | Generate a unique call ID for Gemini tool calls (which lack native IDs)
normalizeToolCallId :: ToolCall -> IO ToolCall
normalizeToolCallId tc = do
  u <- newUnique
  let callId = "call_" <> T.pack (show (hashUnique u))
  pure tc {tcId = callId}

normalizeBlock :: ContentBlock -> IO ContentBlock
normalizeBlock (ToolCallBlock tc) = ToolCallBlock <$> normalizeToolCallId tc
normalizeBlock b = pure b

genConfig :: ChatRequest -> Value
genConfig r =
  object $
    ("maxOutputTokens" .= reqMaxTokens r)
      : ["temperature" .= t | Just t <- [reqTemperature r]]

parseResponse :: Value -> IO LLMResult
parseResponse v = case parseMaybe go v of
  Nothing -> pure $ Left EmptyResponse
  Just blocks -> do
    blocks' <- mapM normalizeBlock blocks
    case blocks' of
      [] -> pure $ Left EmptyResponse
      _ ->
        let text = T.concat [t | TextBlock t <- blocks']
         in pure $ Right (ChatResponse text blocks' (parseUsage v))
  where
    go :: Value -> Parser [ContentBlock]
    go = withObject "GeminiResponse" $ \o -> do
      (cand : _) <- o .: "candidates" :: Parser [Value]
      withObject
        "candidate"
        ( \co -> do
            cont <- co .: "content"
            withObject
              "content"
              ( \cco -> do
                  parts <- cco .: "parts" :: Parser [Value]
                  mapM parsePart parts
              )
              cont
        )
        cand

    parsePart :: Value -> Parser ContentBlock
    parsePart = withObject "part" $ \o -> do
      let tryText = TextBlock <$> (o .: "text")
          tryFunctionCall = do
            fc <- o .: "functionCall"
            withObject
              "functionCall"
              ( \fco -> do
                  name <- fco .: "name"
                  args <- fco .:? "args" .!= object []
                  -- Gemini doesn't provide a call id; use the function name
                  pure $ ToolCallBlock (ToolCall name name args)
              )
              fc
      tryText <|> tryFunctionCall

parseUsage :: Value -> Maybe Usage
parseUsage = parseMaybe $ withObject "GeminiResponse" $ \o -> do
  u <- o .: "usageMetadata"
  withObject
    "usageMetadata"
    (\uo -> Usage <$> uo .: "promptTokenCount" <*> uo .: "candidatesTokenCount")
    u