{-# LANGUAGE DataKinds #-}

module LLM.Providers.Ollama (Ollama (..), ollama, ollamaWith, ollamaProvider, ollamaProviderWith) where

import Control.Applicative ((<|>))
import Data.Aeson
  ( KeyValue ((.=)),
    Value (String),
    decodeStrict',
    encode,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import LLM.Core.LLMProvider (LLMProvider)
import LLM.Core.LLMProviderAdapter (LLMProviderAdapter (..), toProvider)
import LLM.Core.ProviderUtils (handleStreamResponse, lenientConfig)
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
    LLMError (EmptyResponse),
    LLMResult,
    StreamEvent (..),
    ToolCall (..),
    ToolDef (toolDescription, toolName, toolParameters),
    ToolResult (trCallId, trContent),
    Turn (..),
  )
import LLM.Core.Usage (Usage (..))
import LLM.Providers.OpenAI (parseOpenAIResponse, parseOpenAIUsage)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
  ( Option,
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    Scheme (Http),
    Url,
    http,
    jsonResponse,
    port,
    req,
    reqBr,
    responseBody,
    responseStatusCode,
    runReq,
    (/:),
  )

-- | Ollama provider configuration.
-- Connects to a local Ollama instance via its OpenAI-compatible API.
data Ollama = Ollama
  { ollamaBaseUrl :: Url 'Http,
    ollamaBaseOpts :: Option 'Http
  }

-- | Default Ollama provider at localhost:11434.
ollama :: Ollama
ollama = Ollama (http "localhost") (port 11434)

-- | Custom Ollama provider with a different host/port.
ollamaWith :: Url 'Http -> Option 'Http -> Ollama
ollamaWith = Ollama

instance LLMProviderAdapter Ollama where
  providerAdapterName _ = "ollama"

  -- Ollama uses the same request format as OpenAI, but without stream_options
  -- (Ollama doesn't support include_usage in streaming).
  buildBody _ = ollamaBuildBody

  sendRequest (Ollama baseUrl baseOpts) body =
    runReq lenientConfig $ do
      let url = baseUrl /: "v1" /: "chat" /: "completions"
      resp <- req POST url (ReqBodyJson body) jsonResponse baseOpts
      pure (responseStatusCode resp, responseBody resp)

  sendStreamRequest (Ollama baseUrl baseOpts) body callback =
    runReq lenientConfig $ do
      let url = baseUrl /: "v1" /: "chat" /: "completions"
      reqBr POST url (ReqBodyJson body) baseOpts $ \resp ->
        handleStreamResponse resp (`parseOllamaStream` callback)

  parseResponse _ = pure . parseOpenAIResponse

-- | Create a LLMProvider for the default Ollama instance (localhost:11434).
ollamaProvider :: LLMProvider
ollamaProvider = toProvider ollama

-- | Create a LLMProvider for a custom Ollama instance.
ollamaProviderWith :: Url 'Http -> Option 'Http -> LLMProvider
ollamaProviderWith baseUrl baseOpts = toProvider (ollamaWith baseUrl baseOpts)

-- | Build request body — same as OpenAI but without stream_options
-- since Ollama doesn't support include_usage.
ollamaBuildBody :: Bool -> ChatRequest -> Value
ollamaBuildBody stream r =
  object $
    [ "model" .= reqModel r,
      "messages" .= buildMessages r
    ]
      ++ ["num_predict" .= reqMaxTokens r]
      ++ ["temperature" .= t | Just t <- [reqTemperature r]]
      ++ ["tools" .= map encodeToolDef (reqTools r) | not (null (reqTools r))]
      ++ ["stream" .= True | stream]

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

-- | Streaming parser for Ollama.
-- Same format as OpenAI SSE but without usage data in the stream.
parseOllamaStream :: HC.BodyReader -> (StreamEvent -> IO ()) -> IO LLMResult
parseOllamaStream reader callback = do
  blocksRef <- newIORef ([] :: [ContentBlock])
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
                    let cid = fromMaybe "" mId
                        n = fromMaybe "" mName
                     in acc ++ [(idx, cid, n, argChunk)]
                  Just (_, cid, n, a) ->
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
  let text = T.concat [t | TextBlock t <- blocks]
  if null blocks
    then pure $ Left EmptyResponse
    else pure $ Right (ChatResponse text blocks Nothing) -- Ollama doesn't provide usage in streaming

-- Stream chunk parsers (same format as OpenAI)

parseStreamTextDelta :: Value -> Parser Text
parseStreamTextDelta = withObject "chunk" $ \o -> do
  (choice : _) <- o .: "choices"
  withObject "choice" (\co -> co .: "delta" >>= withObject "delta" (\d -> d .:? "content" .!= "")) choice

parseStreamToolDelta :: Value -> Parser (Int, Maybe Text, Maybe Text, Text)
parseStreamToolDelta = withObject "chunk" $ \o -> do
  (choice : _) <- o .: "choices"
  withObject
    "choice"
    ( \co -> do
        delta <- co .: "delta"
        withObject
          "delta"
          ( \d -> do
              (tc : _) <- d .: "tool_calls" :: Parser [Value]
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
          delta
    )
    choice

parseFinishReason :: Value -> Parser Text
parseFinishReason = withObject "chunk" $ \o -> do
  (choice : _) <- o .: "choices"
  withObject "choice" (.: "finish_reason") choice
