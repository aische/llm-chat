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
      "messages" .= map encodeMsg (reqMessages r)
    ]
      ++ ["system" .= sys | Just sys <- [reqSystem r]]
      ++ ["temperature" .= t | Just t <- [reqTemperature r]]

encodeMsg :: Message -> Value
encodeMsg (Message role content) =
  object
    [ "role" .= claudeRole role,
      "content" .= content
    ]

claudeRole :: Role -> Text
claudeRole User = "user"
claudeRole Assistant = "assistant"

parseResponse :: Value -> LLMResult
parseResponse v = case parseMaybe go v of
  Nothing -> Left EmptyResponse
  Just t -> Right (ChatResponse t)
  where
    go :: Value -> Parser Text
    go = withObject "ClaudeResponse" $ \o -> do
      (c : _) <- o .: "content" :: Parser [Value]
      withObject "content_block" (.: "text") c