module LLM.OpenAI (openAIClient, openAIClientWith, parseResponse, parseUsage) where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import LLM.SSE
import LLM.Types
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
import Network.HTTP.Types.Status (statusCode)

-- | Create an OpenAI client for api.openai.com
openAIClient :: Hooks -> Text -> LLMClient
openAIClient = openAIClientWith (https "api.openai.com") mempty

-- | Create an OpenAI-compatible client with a custom base URL.
--
-- Examples:
--
-- @
-- -- Together AI
-- openAIClientWith (https "api.together.xyz") mempty hooks apiKey
--
-- -- Ollama (local)
-- openAIClientWith (http "localhost") (port 11434) hooks ""
--
-- -- vLLM (local)
-- openAIClientWith (http "localhost") (port 8000) hooks ""
-- @
openAIClientWith :: Url scheme -> Option scheme -> Hooks -> Text -> LLMClient
openAIClientWith baseUrl baseOpts hooks apiKey =
  LLMClient
    { clientChat = openAIChat baseUrl baseOpts hooks apiKey,
      clientChatStream = Just (openAIChatStream baseUrl baseOpts hooks apiKey)
    }

openAIChat :: Url scheme -> Option scheme -> Hooks -> Text -> ChatRequest -> IO LLMResult
openAIChat baseUrl baseOpts hooks apiKey r = do
  let reqBody = buildBody False r
  onRequest hooks "openai" reqBody
  result <- try $ runReq lenientConfig $ do
    let url = baseUrl /: "v1" /: "chat" /: "completions"
        opts = baseOpts <> authHeader apiKey
    resp <- req POST url (ReqBodyJson reqBody) jsonResponse opts
    let status = responseStatusCode resp
        body = responseBody resp :: Value
    pure (status, body)
  case result of
    Left e -> pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right (status, body) -> do
      onResponse hooks "openai" body
      pure $
        if status == 200
          then parseResponse body
          else Left $ HttpError status (T.pack $ show body)

openAIChatStream :: Url scheme -> Option scheme -> Hooks -> Text -> ChatRequest -> (StreamEvent -> IO ()) -> IO LLMResult
openAIChatStream baseUrl baseOpts hooks apiKey r callback = do
  let reqBody = buildBody True r
  onRequest hooks "openai" reqBody
  result <- try $ runReq lenientConfig $ do
    let url = baseUrl /: "v1" /: "chat" /: "completions"
        opts = baseOpts <> authHeader apiKey
    reqBr POST url (ReqBodyJson (buildBody True r)) opts $ \resp -> do
      let status = statusCode (HC.responseStatus resp)
      if status /= 200
        then do
          chunks <- readAll (HC.responseBody resp)
          pure $ Left $ HttpError status (decodeUtf8 (BS.concat chunks))
        else parseOpenAIStream (HC.responseBody resp) callback
  case result of
    Left e -> pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right r' -> do
      case r' of
        Right resp -> onResponse hooks "openai" (streamResponseJson resp)
        _ -> pure ()
      pure r'

authHeader :: Text -> Option scheme
authHeader apiKey
  | T.null apiKey = mempty
  | otherwise = header "Authorization" ("Bearer " <> encodeUtf8 apiKey)

lenientConfig :: HttpConfig
lenientConfig =
  defaultHttpConfig
    { httpConfigCheckResponse = \_ _ _ -> Nothing
    }

readAll :: HC.BodyReader -> IO [BS.ByteString]
readAll br = do
  chunk <- HC.brRead br
  if BS.null chunk then pure [] else (chunk :) <$> readAll br

-- Request body

buildBody :: Bool -> ChatRequest -> Value
buildBody stream r =
  object $
    [ "model" .= reqModel r,
      "max_tokens" .= reqMaxTokens r,
      "messages" .= buildMessages r
    ]
      ++ ["temperature" .= t | Just t <- [reqTemperature r]]
      ++ ["tools" .= map encodeToolDef (reqTools r) | not (null (reqTools r))]
      ++ ["stream" .= True | stream]
      ++ ["stream_options" .= object ["include_usage" .= True] | stream]

buildMessages :: ChatRequest -> [Value]
buildMessages r =
  maybe [] (\sys -> [object ["role" .= ("system" :: Text), "content" .= sys]]) (reqSystem r)
    ++ concatMap encodeTurn (reqConversation r)

encodeTurn :: Turn -> [Value]
encodeTurn (UserTurn content) =
  [ object
      [ "role" .= ("user" :: Text),
        "content" .= content
      ]
  ]
encodeTurn (AssistantTurn text calls) =
  [ object $
      ["role" .= ("assistant" :: Text)]
        ++ ["content" .= text | not (T.null text)]
        ++ ["tool_calls" .= map encodeToolCall calls | not (null calls)]
  ]
encodeTurn (ToolTurn results) =
  map encodeToolResult results

encodeToolDef :: ToolDef -> Value
encodeToolDef td =
  object
    [ "type" .= ("function" :: Text),
      "function"
        .= object
          [ "name" .= toolName td,
            "description" .= toolDescription td,
            "parameters" .= toolParameters td
          ]
    ]

encodeToolCall :: ToolCall -> Value
encodeToolCall tc =
  object
    [ "id" .= tcId tc,
      "type" .= ("function" :: Text),
      "function"
        .= object
          [ "name" .= tcName tc,
            "arguments" .= decodeUtf8 (BSL.toStrict (encode (tcArguments tc)))
          ]
    ]

encodeToolResult :: ToolResult -> Value
encodeToolResult tr =
  object
    [ "role" .= ("tool" :: Text),
      "tool_call_id" .= trCallId tr,
      "content" .= trContent tr
    ]

-- Response parsing

parseResponse :: Value -> LLMResult
parseResponse v = case parseMaybe go v of
  Nothing -> Left EmptyResponse
  Just blocks -> case blocks of
    [] -> Left EmptyResponse
    _ ->
      let text = T.concat [t | TextBlock t <- blocks]
       in Right (ChatResponse text blocks (parseUsage v))
  where
    go :: Value -> Parser [ContentBlock]
    go = withObject "OpenAIResponse" $ \o -> do
      (choice : _) <- o .: "choices" :: Parser [Value]
      withObject
        "choice"
        ( \co -> do
            msg <- co .: "message"
            withObject "message" parseMessage msg
        )
        choice

    parseMessage :: Object -> Parser [ContentBlock]
    parseMessage mo = do
      contentBlocks <- do
        mc <- mo .:? "content" :: Parser (Maybe Text)
        pure [TextBlock t | Just t <- [mc], not (T.null t)]
      toolBlocks <- do
        tcs <- mo .:? "tool_calls" .!= [] :: Parser [Value]
        mapM parseToolCall tcs
      pure (contentBlocks ++ toolBlocks)

    parseToolCall :: Value -> Parser ContentBlock
    parseToolCall = withObject "tool_call" $ \tc -> do
      cid <- tc .: "id"
      fn <- tc .: "function"
      withObject
        "function"
        ( \f -> do
            name <- f .: "name"
            argsStr <- f .: "arguments" :: Parser Text
            let args = case decodeStrict' (encodeUtf8 argsStr) of
                  Just v' -> v'
                  Nothing -> String argsStr
            pure $ ToolCallBlock (ToolCall cid name args)
        )
        fn

parseUsage :: Value -> Maybe Usage
parseUsage = parseMaybe $ withObject "OpenAIResponse" $ \o -> do
  u <- o .: "usage"
  withObject "usage" (\uo -> Usage <$> uo .: "prompt_tokens" <*> uo .: "completion_tokens") u

-- Streaming

parseOpenAIStream :: HC.BodyReader -> (StreamEvent -> IO ()) -> IO LLMResult
parseOpenAIStream reader callback = do
  blocksRef <- newIORef ([] :: [ContentBlock])
  usageRef <- newIORef Nothing
  -- Track in-flight tool calls: index -> (id, name, accumulated args)
  toolAccRef <- newIORef ([] :: [(Int, Text, Text, Text)])
  readSSEEvents (HC.brRead reader) $ \sse -> do
    let raw = sseData sse
    if raw == "[DONE]"
      then pure ()
      else case decodeStrict' (encodeUtf8 raw) of
        Nothing -> pure ()
        Just v -> do
          -- Text deltas
          case parseMaybe parseStreamTextDelta v of
            Just txt | not (T.null txt) -> do
              modifyIORef' blocksRef (++ [TextBlock txt])
              callback (StreamDelta txt)
            _ -> pure ()
          -- Tool call deltas
          case parseMaybe parseStreamToolDelta v of
            Just (idx, mId, mName, argChunk) -> do
              modifyIORef' toolAccRef $ \acc ->
                case lookup idx [(i, (i, cid, n, a)) | (i, cid, n, a) <- acc] of
                  Nothing ->
                    -- New tool call
                    let cid = fromMaybe "" mId
                        n = fromMaybe "" mName
                     in acc ++ [(idx, cid, n, argChunk)]
                  Just (_, cid, n, a) ->
                    -- Accumulate arguments
                    [(if i == idx then (i, cid, n, a <> argChunk) else entry) | entry@(i, _, _, _) <- acc]
            Nothing -> pure ()
          -- Finish reason: emit accumulated tool calls
          case parseMaybe parseFinishReason v of
            Just "tool_calls" -> do
              tools <- readIORef toolAccRef
              forM_ tools $ \(_, cid, name, argsStr) -> do
                let args = case decodeStrict' (encodeUtf8 argsStr) of
                      Just a -> a
                      Nothing -> String argsStr
                    tc = ToolCall cid name args
                modifyIORef' blocksRef (++ [ToolCallBlock tc])
                callback (StreamToolCall tc)
              writeIORef toolAccRef []
            _ -> pure ()
          -- Usage (in the final chunk when stream_options.include_usage is set)
          case parseMaybe parseStreamUsage v of
            Just u -> writeIORef usageRef (Just u)
            Nothing -> pure ()
  -- Flush any remaining tool calls
  tools <- readIORef toolAccRef
  forM_ tools $ \(_, cid, name, argsStr) -> do
    let args = case decodeStrict' (encodeUtf8 argsStr) of
          Just a -> a
          Nothing -> String argsStr
        tc = ToolCall cid name args
    modifyIORef' blocksRef (++ [ToolCallBlock tc])
    callback (StreamToolCall tc)
  blocks <- readIORef blocksRef
  usage <- readIORef usageRef
  let text = T.concat [t | TextBlock t <- blocks]
  if null blocks
    then pure $ Left EmptyResponse
    else pure $ Right (ChatResponse text blocks usage)
  where
    forM_ xs f = mapM_ f xs

parseStreamTextDelta :: Value -> Parser Text
parseStreamTextDelta = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject "choice" (\co -> do d <- co .: "delta"; withObject "delta" (.: "content") d) c

parseStreamToolDelta :: Value -> Parser (Int, Maybe Text, Maybe Text, Text)
parseStreamToolDelta = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject
    "choice"
    ( \co -> do
        d <- co .: "delta"
        withObject
          "delta"
          ( \d' -> do
              (tc : _) <- d' .: "tool_calls" :: Parser [Value]
              withObject
                "tool_call"
                ( \tco -> do
                    idx <- tco .: "index"
                    mId <- tco .:? "id"
                    fn <- tco .:? "function" .!= object []
                    withObject
                      "function"
                      ( \f -> do
                          mName <- f .:? "name"
                          args <- f .:? "arguments" .!= ""
                          pure (idx, mId, mName, args)
                      )
                      fn
                )
                tc
          )
          d
    )
    c

parseFinishReason :: Value -> Parser Text
parseFinishReason = withObject "chunk" $ \o -> do
  (c : _) <- o .: "choices" :: Parser [Value]
  withObject "choice" (.: "finish_reason") c

parseStreamUsage :: Value -> Parser Usage
parseStreamUsage = withObject "chunk" $ \o -> do
  u <- o .: "usage"
  withObject "usage" (\uo -> Usage <$> uo .: "prompt_tokens" <*> uo .: "completion_tokens") u
