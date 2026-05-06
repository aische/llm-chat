{-# LANGUAGE LambdaCase #-}

module LLM.Providers.Gemini (Gemini (..), geminiProvider, parseGeminiResponse, parseGeminiUsage) where

import Control.Applicative ((<|>))
import Data.Aeson
  ( KeyValue ((.=)),
    Value (Object),
    decodeStrict',
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Unique (hashUnique, newUnique)
import LLM.Core.LLMProvider (LLMProvider)
import LLM.Core.LLMProviderAdapter (LLMProviderAdapter (..), toProvider)
import LLM.Core.ProviderUtils (handleStreamResponse, lenientConfig, stripJsonFences)
import LLM.Core.SSE (SSEEvent (sseData), readSSEEvents)
import LLM.Core.Types
  ( ChatRequest
      ( reqConversation,
        reqMaxTokens,
        reqModel,
        reqSystem,
        reqTemperature,
        reqTools
      ),
    ChatResponse (ChatResponse),
    ContentBlock (..),
    Conversation (unConversation),
    LLMError (EmptyResponse),
    LLMObjectResult,
    LLMRes (ResError, ResOk),
    LLMResult (..),
    StreamEvent (..),
    ToolCall (..),
    ToolDef (toolDescription, toolName, toolParameters),
    ToolResult (trContent, trName),
    Turn (..),
  )
import LLM.Core.Usage (Usage (..))
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
  ( POST (POST),
    ReqBodyJson (ReqBodyJson),
    https,
    jsonResponse,
    req,
    reqBr,
    responseBody,
    responseStatusCode,
    runReq,
    (/:),
    (=:),
  )

-- | Gemini provider configuration
newtype Gemini = Gemini
  { geminiApiKey :: Text
  }

instance LLMProviderAdapter Gemini where
  providerAdapterName _ = "gemini"

  buildBody _ _ = geminiBuildBody

  sendRequest (Gemini apiKey) body =
    runReq lenientConfig $ do
      -- For non-streaming we need the model name from the body to construct the URL.
      -- We extract it from the request body JSON since the typeclass only passes Value.
      let model = extractModel body
          url =
            https "generativelanguage.googleapis.com"
              /: "v1beta"
              /: "models"
              /: (model <> ":generateContent")
      resp <- req POST url (ReqBodyJson (stripModel body)) jsonResponse ("key" =: apiKey)
      pure (responseStatusCode resp, responseBody resp)

  sendStreamRequest (Gemini apiKey) body callback =
    runReq lenientConfig $ do
      let model = extractModel body
          url =
            https "generativelanguage.googleapis.com"
              /: "v1beta"
              /: "models"
              /: (model <> ":streamGenerateContent")
      reqBr POST url (ReqBodyJson (stripModel body)) ("key" =: apiKey <> "alt" =: ("sse" :: Text)) $ \resp ->
        handleStreamResponse resp (`parseGeminiStream` callback)

  parseResponse :: Gemini -> Value -> IO LLMResult
  parseResponse _ = parseGeminiResponse

  -- buildObjectBody :: Gemini -> ChatRequest -> Value -> Value
  -- buildObjectBody _ r schema = object (geminiBuildBodyPairs r <> ["generationConfig" .= object ["responseSchema" .= schema]])

  -- buildObjectBody _ r schema = object (geminiBuildBodyPairs r <> ["generationConfig" .= object ["responseMimeType" .= ("application/json" :: Text), "responseSchema" .= schema]])

  buildObjectBody _ r schema =
    object $
      [ "_model" .= reqModel r,
        "contents" .= concatMap encodeTurn (unConversation $ reqConversation r),
        "generationConfig"
          .= object
            ( [ "maxOutputTokens" .= reqMaxTokens r,
                "responseMimeType" .= ("application/json" :: Text),
                "responseSchema" .= schema
              ]
                ++ ["temperature" .= t | Just t <- [reqTemperature r]]
            )
      ]
        ++ [ "system_instruction" .= object ["parts" .= [object ["text" .= sys]]]
             | Just sys <- [reqSystem r]
           ]
        ++ [ "tools" .= [object ["function_declarations" .= map encodeToolDef (reqTools r)]]
             | not (null (reqTools r))
           ]
  sendObjectRequest = sendRequest

  parseObjectResponse _ = parseGeminiObjectResponse

-- | Create an LLMClient from Gemini credentials
geminiProvider :: Text -> LLMProvider
geminiProvider apiKey = toProvider (Gemini apiKey)

-- | Extract model name stashed in the request body by geminiBuildBody.
extractModel :: Value -> Text
extractModel v = fromMaybe "gemini-2.0-flash" (parseMaybe (withObject "body" (.: "_model")) v)

-- | Remove the internal '_model' field before sending to the API.
stripModel :: Value -> Value
stripModel (Object o) = Object (KM.delete "_model" o)
stripModel v = v

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
    then pure $ ResError EmptyResponse
    else pure $ ResOk (ChatResponse text blocks usage)
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
        (\uo -> Usage <$> uo .: "promptTokenCount" <*> uo .: "candidatesTokenCount" <*> pure 0)
        u

geminiBuildBody :: ChatRequest -> Value
geminiBuildBody r = object $ geminiBuildBodyPairs r

geminiBuildBodyPairs :: ChatRequest -> [Pair]
geminiBuildBodyPairs r =
  [ "_model" .= reqModel r,
    "contents" .= concatMap encodeTurn (unConversation $ reqConversation r),
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

parseGeminiResponse :: Value -> IO LLMResult
parseGeminiResponse v = case parseMaybe go v of
  Nothing -> pure $ ResError EmptyResponse
  Just blocks -> do
    blocks' <- mapM normalizeBlock blocks
    case blocks' of
      [] -> pure $ ResError EmptyResponse
      _ ->
        let text = T.concat [t | TextBlock t <- blocks']
         in pure $ ResOk (ChatResponse text blocks' (parseGeminiUsage v))
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

parseGeminiUsage :: Value -> Maybe Usage
parseGeminiUsage = parseMaybe $ withObject "GeminiResponse" $ \o -> do
  u <- o .: "usageMetadata"
  withObject
    "usageMetadata"
    (\uo -> Usage <$> uo .: "promptTokenCount" <*> uo .: "candidatesTokenCount" <*> pure 0)
    u

parseGeminiObjectResponse :: Value -> IO LLMObjectResult
parseGeminiObjectResponse v = case parseMaybe go v of
  Nothing -> pure $ ResError EmptyResponse
  Just text -> case decodeStrict' (encodeUtf8 (stripJsonFences text)) of
    Nothing -> pure $ ResError EmptyResponse
    Just obj -> pure $ ResOk obj
  where
    go :: Value -> Parser Text
    go = withObject "GeminiObjectResponse" $ \o -> do
      (cand : _) <- o .: "candidates" :: Parser [Value]
      withObject "candidate" (\co -> co .: "content" >>= withObject "content" (\cco -> cco .: "parts" >>= \case (p : _) -> withObject "part" (.: "text") p; _ -> fail "No parts")) cand