module LLM.Gemini (geminiClient) where

import Control.Applicative ((<|>))
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
    [ "contents" .= buildContents r,
      "generationConfig" .= genConfig r
    ]
      ++ [ "system_instruction" .= object ["parts" .= [object ["text" .= sys]]]
           | Just sys <- [reqSystem r]
         ]
      ++ [ "tools" .= [object ["function_declarations" .= map encodeToolDef (reqTools r)]]
           | not (null (reqTools r))
         ]

buildContents :: ChatRequest -> [Value]
buildContents r =
  map encodeMsg (reqMessages r)
    ++ [ encodeModelFunctionCalls (reqPendingToolCalls r)
         | not (null (reqPendingToolCalls r))
       ]
    ++ [ encodeFunctionResponses (reqToolResults r)
         | not (null (reqToolResults r))
       ]

encodeMsg :: Message -> Value
encodeMsg (Message role content) =
  object
    [ "role" .= geminiRole role,
      "parts" .= [object ["text" .= content]]
    ]

encodeToolDef :: ToolDef -> Value
encodeToolDef td =
  object
    [ "name" .= toolName td,
      "description" .= toolDescription td,
      "parameters" .= toolParameters td
    ]

encodeModelFunctionCalls :: [ToolCall] -> Value
encodeModelFunctionCalls tcs =
  object
    [ "role" .= ("model" :: Text),
      "parts" .= map encodeFunctionCall tcs
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

encodeFunctionResponses :: [ToolResult] -> Value
encodeFunctionResponses trs =
  object
    [ "role" .= ("user" :: Text),
      "parts" .= map encodeFunctionResponse trs
    ]

encodeFunctionResponse :: ToolResult -> Value
encodeFunctionResponse tr =
  object
    [ "functionResponse"
        .= object
          [ "name" .= trCallId tr, -- for Gemini, trCallId stores the function name
            "response" .= object ["result" .= trContent tr]
          ]
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
  Just blocks -> case blocks of
    [] -> Left EmptyResponse
    _ ->
      let text = T.concat [t | TextBlock t <- blocks]
       in Right (ChatResponse text blocks)
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