module LLM.Gemini (geminiClient) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Types
import Network.HTTP.Req

geminiClient :: Text -> LLMClient
geminiClient apiKey = LLMClient (geminiChat apiKey)

geminiChat :: Text -> ChatRequest -> IO LLMResult
geminiChat apiKey r = runReq lenientConfig $ do
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
      then parseResponse body
      else Left $ HttpError status (T.pack $ show body)

-- Don't let req throw on non-2xx; we handle it ourselves
lenientConfig :: HttpConfig
lenientConfig =
  defaultHttpConfig
    { httpConfigCheckResponse = \_ _ _ -> Nothing
    }

buildBody :: ChatRequest -> Value
buildBody r =
  object $
    [ "contents" .= map encodeMsg (reqMessages r),
      "generationConfig" .= genConfig r
    ]
      ++
      -- list comprehension as optional field — a nice Haskell idiom
      [ "system_instruction" .= object ["parts" .= [object ["text" .= sys]]]
        | Just sys <- [reqSystem r]
      ]

encodeMsg :: Message -> Value
encodeMsg (Message role content) =
  object
    [ "role" .= geminiRole role,
      "parts" .= [object ["text" .= content]]
    ]

geminiRole :: Role -> Text
geminiRole User = "user"
geminiRole Assistant = "model" -- Gemini calls it "model", not "assistant"

genConfig :: ChatRequest -> Value
genConfig r =
  object $
    ("maxOutputTokens" .= reqMaxTokens r)
      : ["temperature" .= t | Just t <- [reqTemperature r]]

parseResponse :: Value -> LLMResult
parseResponse v = case parseMaybe go v of
  Nothing -> Left EmptyResponse
  Just t -> Right (ChatResponse t)
  where
    go :: Value -> Parser Text
    go = withObject "GeminiResponse" $ \o -> do
      (cand : _) <- o .: "candidates" :: Parser [Value]
      withObject
        "candidate"
        ( \co -> do
            cont <- co .: "content"
            withObject
              "content"
              ( \cco -> do
                  (part : _) <- cco .: "parts" :: Parser [Value]
                  withObject "part" (.: "text") part
              )
              cont
        )
        cand