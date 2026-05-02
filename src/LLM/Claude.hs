module LLM.Claude (claudeClient) where

import Control.Exception (try)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import LLM.SSE
import LLM.Types
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
import Network.HTTP.Types.Status (statusCode)

claudeClient :: Text -> LLMClient
claudeClient apiKey =
  LLMClient
    { clientChat = claudeChat apiKey,
      clientChatStream = Just (claudeChatStream apiKey)
    }

claudeUrl = https "api.anthropic.com" /: "v1" /: "messages"

claudeOpts apiKey =
  header "x-api-key" (encodeUtf8 apiKey)
    <> header "anthropic-version" "2023-06-01"

claudeChat :: Text -> ChatRequest -> IO LLMResult
claudeChat apiKey r = do
  result <- try $ runReq lenientConfig $ do
    resp <- req POST claudeUrl (ReqBodyJson (buildBody False r)) jsonResponse (claudeOpts apiKey)
    let status = responseStatusCode resp
        body = responseBody resp :: Value
    pure $
      if status == 200
        then parseResponse body
        else Left $ HttpError status (T.pack $ show body)
  case result of
    Left e -> pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right r' -> pure r'

claudeChatStream :: Text -> ChatRequest -> (StreamEvent -> IO ()) -> IO LLMResult
claudeChatStream apiKey r callback = do
  result <- try $ runReq lenientConfig $ do
    reqBr POST claudeUrl (ReqBodyJson (buildBody True r)) (claudeOpts apiKey) $ \resp -> do
      let status = statusCode (HC.responseStatus resp)
      if status /= 200
        then do
          chunks <- readAll (HC.responseBody resp)
          pure $ Left $ HttpError status (decodeUtf8 (BS.concat chunks))
        else parseClaudeStream (HC.responseBody resp) callback
  case result of
    Left e -> pure $ Left $ NetworkError (T.pack (show (e :: HttpException)))
    Right r' -> pure r'

-- Accumulate all chunks from a BodyReader for error messages
readAll :: HC.BodyReader -> IO [BS.ByteString]
readAll br = do
  chunk <- HC.brRead br
  if BS.null chunk then pure [] else (chunk :) <$> readAll br

parseClaudeStream :: HC.BodyReader -> (StreamEvent -> IO ()) -> IO LLMResult
parseClaudeStream reader callback = do
  blocksRef <- newIORef ([] :: [ContentBlock])
  usageRef <- newIORef emptyUsage
  -- For accumulating tool_use input JSON across deltas
  toolAccRef <- newIORef (Nothing :: Maybe (Text, Text, Text)) -- (id, name, json_so_far)
  readSSEEvents (HC.brRead reader) $ \sse -> do
    case sseEvent sse of
      Just "message_start" ->
        -- Extract input token count from message.usage
        case decodeStrict' (encodeUtf8 (sseData sse)) of
          Just v -> case parseMaybe parseMessageStartUsage v of
            Just inputToks -> modifyIORef' usageRef $ \u -> u {usageInputTokens = inputToks}
            Nothing -> pure ()
          Nothing -> pure ()
      Just "content_block_start" ->
        case decodeStrict' (encodeUtf8 (sseData sse)) of
          Just v -> case parseMaybe parseContentBlockStart v of
            Just (cid, name) -> writeIORef toolAccRef (Just (cid, name, ""))
            Nothing -> pure () -- text block start, nothing to do
          Nothing -> pure ()
      Just "content_block_delta" ->
        case decodeStrict' (encodeUtf8 (sseData sse)) of
          Just v -> do
            -- Try text delta
            case parseMaybe parseTextDelta v of
              Just txt -> do
                modifyIORef' blocksRef (++ [TextBlock txt])
                callback (StreamDelta txt)
              Nothing -> pure ()
            -- Try tool input delta
            case parseMaybe parseInputJsonDelta v of
              Just fragment ->
                modifyIORef' toolAccRef $ fmap (\(cid, name, acc) -> (cid, name, acc <> fragment))
              Nothing -> pure ()
          Nothing -> pure ()
      Just "content_block_stop" -> do
        mTool <- readIORef toolAccRef
        case mTool of
          Just (cid, name, jsonStr) | not (T.null jsonStr) -> do
            let args = case decodeStrict' (encodeUtf8 jsonStr) of
                  Just v -> v
                  Nothing -> String jsonStr
                tc = ToolCall cid name args
            modifyIORef' blocksRef (++ [ToolCallBlock tc])
            callback (StreamToolCall tc)
            writeIORef toolAccRef Nothing
          _ -> writeIORef toolAccRef Nothing
      Just "message_delta" ->
        case decodeStrict' (encodeUtf8 (sseData sse)) of
          Just v -> case parseMaybe parseMessageDeltaUsage v of
            Just outputToks -> modifyIORef' usageRef $ \u -> u {usageOutputTokens = outputToks}
            Nothing -> pure ()
          Nothing -> pure ()
      _ -> pure () -- message_stop, ping, etc.
  blocks <- readIORef blocksRef
  usage <- readIORef usageRef
  let text = T.concat [t | TextBlock t <- blocks]
  if null blocks
    then pure $ Left EmptyResponse
    else pure $ Right (ChatResponse text blocks (Just usage))

-- Parsers for streaming events
parseMessageStartUsage :: Value -> Parser Int
parseMessageStartUsage = withObject "message_start" $ \o -> do
  msg <- o .: "message"
  withObject "message" (\mo -> do u <- mo .: "usage"; withObject "usage" (.: "input_tokens") u) msg

parseMessageDeltaUsage :: Value -> Parser Int
parseMessageDeltaUsage = withObject "message_delta" $ \o -> do
  u <- o .: "usage"
  withObject "usage" (.: "output_tokens") u

parseContentBlockStart :: Value -> Parser (Text, Text)
parseContentBlockStart = withObject "content_block_start" $ \o -> do
  cb <- o .: "content_block"
  withObject
    "content_block"
    ( \cbo -> do
        typ <- cbo .: "type" :: Parser Text
        case typ of
          "tool_use" -> (,) <$> cbo .: "id" <*> cbo .: "name"
          _ -> fail "not tool_use"
    )
    cb

parseTextDelta :: Value -> Parser Text
parseTextDelta = withObject "delta_event" $ \o -> do
  d <- o .: "delta"
  withObject
    "delta"
    ( \d' -> do
        typ <- d' .: "type" :: Parser Text
        case typ of
          "text_delta" -> d' .: "text"
          _ -> fail "not text_delta"
    )
    d

parseInputJsonDelta :: Value -> Parser Text
parseInputJsonDelta = withObject "delta_event" $ \o -> do
  d <- o .: "delta"
  withObject
    "delta"
    ( \d' -> do
        typ <- d' .: "type" :: Parser Text
        case typ of
          "input_json_delta" -> d' .: "partial_json"
          _ -> fail "not input_json_delta"
    )
    d

lenientConfig :: HttpConfig
lenientConfig =
  defaultHttpConfig
    { httpConfigCheckResponse = \_ _ _ -> Nothing
    }

buildBody :: Bool -> ChatRequest -> Value
buildBody stream r =
  object $
    [ "model" .= reqModel r,
      "max_tokens" .= reqMaxTokens r,
      "messages" .= concatMap encodeTurn (reqConversation r)
    ]
      ++ ["system" .= sys | Just sys <- [reqSystem r]]
      ++ ["temperature" .= t | Just t <- [reqTemperature r]]
      ++ ["tools" .= map encodeToolDef (reqTools r) | not (null (reqTools r))]
      ++ ["stream" .= True | stream]

encodeTurn :: Turn -> [Value]
encodeTurn (UserTurn content) =
  [ object
      [ "role" .= ("user" :: Text),
        "content" .= content
      ]
  ]
encodeTurn (AssistantTurn text calls) =
  [ object
      [ "role" .= ("assistant" :: Text),
        "content" .= (textBlocks ++ toolBlocks)
      ]
  ]
  where
    textBlocks = [object ["type" .= ("text" :: Text), "text" .= text] | not (T.null text)]
    toolBlocks = map encodeToolUseBlock calls
encodeTurn (ToolTurn results) =
  [ object
      [ "role" .= ("user" :: Text),
        "content" .= map encodeToolResult results
      ]
  ]

encodeToolDef :: ToolDef -> Value
encodeToolDef td =
  object
    [ "name" .= toolName td,
      "description" .= toolDescription td,
      "input_schema" .= toolParameters td
    ]

encodeToolUseBlock :: ToolCall -> Value
encodeToolUseBlock tc =
  object
    [ "type" .= ("tool_use" :: Text),
      "id" .= tcId tc,
      "name" .= tcName tc,
      "input" .= tcArguments tc
    ]

encodeToolResult :: ToolResult -> Value
encodeToolResult tr =
  object
    [ "type" .= ("tool_result" :: Text),
      "tool_use_id" .= trCallId tr,
      "content" .= trContent tr
    ]

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
    go = withObject "ClaudeResponse" $ \o -> do
      content <- o .: "content" :: Parser [Value]
      mapM parseBlock content

    parseBlock :: Value -> Parser ContentBlock
    parseBlock = withObject "content_block" $ \o -> do
      typ <- o .: "type" :: Parser Text
      case typ of
        "text" -> TextBlock <$> o .: "text"
        "tool_use" -> do
          cid <- o .: "id"
          name <- o .: "name"
          args <- o .: "input"
          pure $ ToolCallBlock (ToolCall cid name args)
        _ -> fail $ "Unknown content block type: " <> T.unpack typ

parseUsage :: Value -> Maybe Usage
parseUsage = parseMaybe $ withObject "ClaudeResponse" $ \o -> do
  u <- o .: "usage"
  withObject "usage" (\uo -> Usage <$> uo .: "input_tokens" <*> uo .: "output_tokens") u