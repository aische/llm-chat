{-# LANGUAGE DataKinds #-}

module LLM.Providers.Ollama (Ollama (..), ollama, ollamaWith, ollamaProvider, ollamaProviderWith) where

import Data.Aeson
  ( KeyValue ((.=)),
    Value,
    decodeStrict',
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import LLM.Core.LLMProvider (LLMProvider)
import LLM.Core.LLMProviderAdapter (LLMProviderAdapter (..), toProvider)
import LLM.Core.ProviderUtils (handleStreamResponse, lenientConfig, normalizeSchemaOpenAI)
import LLM.Core.Types
  ( ChatRequest
      ( reqMaxTokens,
        reqModel,
        reqTemperature,
        reqTools
      ),
    LLMError (EmptyResponse),
  )
import LLM.Providers.OpenAI (buildMessages, encodeToolDef, openAIBuildBodyPairs, parseOpenAIResponse, parseOpenAIStream)
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
    Nothing -> pure $ Left EmptyResponse
    Just contentStr -> case decodeStrict' (encodeUtf8 contentStr) of
      Nothing -> pure $ Left EmptyResponse
      Just obj -> pure $ Right obj
    where
      parseObject :: Value -> Parser Text
      parseObject = withObject "OpenAIObjectResponse" $ \o -> do
        (choice : _) <- o .: "choices" :: Parser [Value]
        withObject "choice" (\co -> co .: "message" >>= withObject "message" (.: "content")) choice

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
