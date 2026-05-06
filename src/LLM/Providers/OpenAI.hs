module LLM.Providers.OpenAI
  ( OpenAI,
    openAI,
    openAIWith,
    openAIProvider,
    openAIProviderWith,
    parseOpenAIResponse,
    parseOpenAIUsage,
    buildMessages,
    encodeToolDef,
    parseOpenAIStream,
    openAIBuildBody,
    openAIBuildBodyPairs,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
  ( KeyValue ((.=)),
    Object,
    Value (String),
    decodeStrict',
    encode,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import LLM.Core.LLMProvider (LLMProvider)
import LLM.Core.LLMProviderAdapter (LLMProviderAdapter (..), toProvider)
import LLM.Core.ProviderUtils (handleStreamResponse, lenientConfig, normalizeSchemaOpenAI, stripJsonFences)
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
    ToolResult (trCallId, trContent),
    Turn (..),
  )
import LLM.Core.Usage (Usage (..))
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
  ( Option,
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Url,
    header,
    https,
    jsonResponse,
    req,
    reqBr,
    responseBody,
    responseStatusCode,
    runReq,
    (/:),
  )

-- | OpenAI provider configuration.
-- Uses an existential to support both Http and Https schemes.
data OpenAI = forall scheme. OpenAI
  { openAIBaseUrl :: Url scheme,
    openAIBaseOpts :: Option scheme,
    openAIApiKey :: Text
  }

-- | Create an OpenAI provider for api.openai.com
openAI :: Text -> OpenAI
openAI = OpenAI (https "api.openai.com") mempty

-- | Create an OpenAI-compatible provider with a custom base URL.
--
-- Examples:
--
-- @
-- -- Together AI
-- openAIWith (https "api.together.xyz") mempty apiKey
--
-- -- Ollama (local)
-- openAIWith (http "localhost") (port 11434) ""
--
-- -- vLLM (local)
-- openAIWith (http "localhost") (port 8000) ""
-- @
openAIWith :: Url scheme -> Option scheme -> Text -> OpenAI
openAIWith = OpenAI

instance LLMProviderAdapter OpenAI where
  providerAdapterName _ = "openai"

  buildBody _ = openAIBuildBody

  sendRequest (OpenAI baseUrl baseOpts apiKey) body =
    runReq lenientConfig $ do
      let url = baseUrl /: "v1" /: "chat" /: "completions"
          opts = baseOpts <> authHeader apiKey
      resp <- req POST url (ReqBodyJson body) jsonResponse opts
      pure (responseStatusCode resp, responseBody resp)

  sendStreamRequest (OpenAI baseUrl baseOpts apiKey) body callback =
    runReq lenientConfig $ do
      let url = baseUrl /: "v1" /: "chat" /: "completions"
          opts = baseOpts <> authHeader apiKey
      reqBr POST url (ReqBodyJson body) opts $ \resp ->
        handleStreamResponse resp (`parseOpenAIStream` callback)

  parseResponse _ = pure . parseOpenAIResponse
  buildObjectBody _ r schema =
    object $
      openAIBuildBodyPairs False r
        <> [ "response_format"
               .= object
                 [ "type" .= ("json_schema" :: Text),
                   "json_schema"
                     .= object
                       [ "name" .= ("response" :: Text),
                         "schema" .= normalizeSchemaOpenAI schema,
                         "strict" .= True
                       ]
                 ]
           ]

  sendObjectRequest = sendRequest

  parseObjectResponse _ v = case parseMaybe parseObject v of
    Nothing -> pure $ ResError EmptyResponse
    Just contentStr -> case decodeStrict' (encodeUtf8 (stripJsonFences contentStr)) of
      Nothing -> pure $ ResError EmptyResponse
      Just obj -> pure $ ResOk obj
    where
      parseObject :: Value -> Parser Text
      parseObject = withObject "OpenAIObjectResponse" $ \o -> do
        (choice : _) <- o .: "choices" :: Parser [Value]
        withObject "choice" (\co -> co .: "message" >>= withObject "message" (.: "content")) choice

-- | Create an OpenAI client for api.openai.com
openAIProvider :: Text -> LLMProvider
openAIProvider apiKey = toProvider (openAI apiKey)

-- | Create an OpenAI-compatible client with a custom base URL.
openAIProviderWith :: Url scheme -> Option scheme -> Text -> LLMProvider
openAIProviderWith baseUrl baseOpts apiKey = toProvider (openAIWith baseUrl baseOpts apiKey)

authHeader :: Text -> Option scheme
authHeader apiKey
  | T.null apiKey = mempty
  | otherwise = header "Authorization" ("Bearer " <> encodeUtf8 apiKey)

-- Request body

openAIBuildBody :: Bool -> ChatRequest -> Value
openAIBuildBody stream r = object $ openAIBuildBodyPairs stream r

openAIBuildBodyPairs :: Bool -> ChatRequest -> [Pair]
openAIBuildBodyPairs stream r =
  [ "model" .= reqModel r,
    "max_completion_tokens" .= reqMaxTokens r,
    "messages" .= buildMessages r
  ]
    ++ ["temperature" .= t | Just t <- [reqTemperature r]]
    ++ ["tools" .= map encodeToolDef (reqTools r) | not (null (reqTools r))]
    ++ ["stream" .= True | stream]
    ++ ["stream_options" .= object ["include_usage" .= True] | stream]

buildMessages :: ChatRequest -> [Value]
buildMessages r =
  maybe [] (\sys -> [object ["role" .= ("system" :: Text), "content" .= sys]]) (reqSystem r)
    ++ concatMap encodeTurn (unConversation $ reqConversation r)

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

parseOpenAIResponse :: Value -> LLMResult
parseOpenAIResponse v = case parseMaybe go v of
  Nothing -> ResError EmptyResponse
  Just blocks -> case blocks of
    [] -> ResError EmptyResponse
    _ ->
      let text = T.concat [t | TextBlock t <- blocks]
       in ResOk (ChatResponse text blocks (parseOpenAIUsage v))
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

parseOpenAIUsage :: Value -> Maybe Usage
parseOpenAIUsage = parseMaybe $ withObject "OpenAIResponse" $ \o -> do
  u <- o .: "usage"
  withObject "usage" (\uo -> Usage <$> uo .: "prompt_tokens" <*> uo .: "completion_tokens" <*> pure 0) u

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
              modifyIORef' blocksRef (TextBlock txt :)
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
                modifyIORef' blocksRef (ToolCallBlock tc :)
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
    modifyIORef' blocksRef (ToolCallBlock tc :)
    callback (StreamToolCall tc)
  blocks <- reverse <$> readIORef blocksRef
  usage <- readIORef usageRef
  let text = T.concat [t | TextBlock t <- blocks]
  if null blocks
    then pure $ ResError EmptyResponse
    else pure $ ResOk (ChatResponse text blocks usage)

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
  withObject "usage" (\uo -> Usage <$> uo .: "prompt_tokens" <*> uo .: "completion_tokens" <*> pure 0) u
