module LLM.Claude (claudeClient) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import LLM.Types
import Network.HTTP.Req

claudeClient :: Text -> LLMClient
claudeClient apiKey = LLMClient (claudeChat apiKey)

claudeChat :: Text -> ChatRequest -> IO LLMResult
claudeChat apiKey r = runReq lenientConfig $ do
  let url = https "api.anthropic.com" /: "v1" /: "messages"
      opts =
        header "x-api-key" (encodeUtf8 apiKey)
          <> header "anthropic-version" "2023-06-01"
  resp <- req POST url (ReqBodyJson (buildBody r)) jsonResponse opts
  let status = responseStatusCode resp
      body = responseBody resp :: Value
  pure $
    if status == 200
      then parseResponse body
      else Left $ HttpError status (T.pack $ show body)

lenientConfig :: HttpConfig
lenientConfig =
  defaultHttpConfig
    { httpConfigCheckResponse = \_ _ _ -> Nothing
    }

buildBody :: ChatRequest -> Value
buildBody r =
  object $
    [ "model" .= reqModel r,
      "max_tokens" .= reqMaxTokens r,
      "messages" .= buildMessages r
    ]
      ++ ["system" .= sys | Just sys <- [reqSystem r]]
      ++ ["temperature" .= t | Just t <- [reqTemperature r]]
      ++ ["tools" .= map encodeToolDef (reqTools r) | not (null (reqTools r))]

buildMessages :: ChatRequest -> [Value]
buildMessages r =
  map encodeMsg (reqMessages r)
    ++ [ encodeAssistantToolUse (reqPendingToolCalls r)
         | not (null (reqPendingToolCalls r))
       ]
    ++ [ encodeToolResults (reqToolResults r)
         | not (null (reqToolResults r))
       ]

encodeMsg :: Message -> Value
encodeMsg (Message role content) =
  object
    [ "role" .= claudeRole role,
      "content" .= content
    ]

encodeToolDef :: ToolDef -> Value
encodeToolDef td =
  object
    [ "name" .= toolName td,
      "description" .= toolDescription td,
      "input_schema" .= toolParameters td
    ]

encodeAssistantToolUse :: [ToolCall] -> Value
encodeAssistantToolUse tcs =
  object
    [ "role" .= ("assistant" :: Text),
      "content" .= map encodeToolUseBlock tcs
    ]

encodeToolUseBlock :: ToolCall -> Value
encodeToolUseBlock tc =
  object
    [ "type" .= ("tool_use" :: Text),
      "id" .= tcId tc,
      "name" .= tcName tc,
      "input" .= tcArguments tc
    ]

encodeToolResults :: [ToolResult] -> Value
encodeToolResults trs =
  object
    [ "role" .= ("user" :: Text),
      "content" .= map encodeToolResult trs
    ]

encodeToolResult :: ToolResult -> Value
encodeToolResult tr =
  object
    [ "type" .= ("tool_result" :: Text),
      "tool_use_id" .= trCallId tr,
      "content" .= trContent tr
    ]

claudeRole :: Role -> Text
claudeRole User = "user"
claudeRole Assistant = "assistant"

parseResponse :: Value -> LLMResult
parseResponse v = case parseMaybe go v of
  Nothing -> Left EmptyResponse
  Just blocks -> case blocks of
    [] -> Left EmptyResponse
    _ ->
      let text = T.concat [t | TextBlock t <- blocks]
       in Right (ChatResponse text blocks)
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