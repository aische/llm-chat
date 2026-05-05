module LLM.Core.LLMProviderAdapter
  ( LLMProviderAdapter (..),
    toProvider,
    genericChat,
    genericStreamChat,
  )
where

import Control.Exception (try)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.LLMProvider (LLMProvider (..))
import LLM.Core.Logger (Hooks (..))
import LLM.Core.Types
  ( ChatRequest,
    LLMError (HttpError, NetworkError),
    LLMResult (..),
    StreamEvent,
  )
import LLM.Core.Utils (streamResponseJson)
import Network.HTTP.Req (HttpException)

-- | Typeclass for LLM provider backends.
--
-- Each provider implements how to build requests, send them over HTTP,
-- and parse responses. The generic chat functions handle hooks, error
-- wrapping, and retries uniformly.
class LLMProviderAdapter a where
  -- | Provider name for logging/hooks (e.g. "claude", "gemini", "openai")
  providerAdapterName :: a -> Text

  -- | Build the JSON request body. Bool indicates whether streaming is requested.
  buildBody :: a -> Bool -> ChatRequest -> Value

  -- | Make a non-streaming HTTP call, returning (status code, response JSON).
  sendRequest :: a -> Value -> IO (Int, Value)

  -- | Make a streaming HTTP call. The handler receives the raw response
  -- and should parse it (checking status, reading body, etc.).
  sendStreamRequest :: a -> Value -> (StreamEvent -> IO ()) -> IO LLMResult

  -- | Parse a complete (non-streaming) JSON response body.
  parseResponse :: a -> Value -> IO LLMResult

  -- | Build the JSON request body for object generation.
  buildObjectBody :: a -> ChatRequest -> Value -> Value

  -- | Make a non-streaming HTTP call for object generation, returning (status code, response JSON).
  sendObjectRequest :: a -> Value -> IO (Int, Value)

  -- | Parse a complete JSON response body for object generation.
  parseObjectResponse :: a -> Value -> IO LLMResult

-- | Generic non-streaming chat via the typeclass.
genericChat :: (LLMProviderAdapter a) => a -> Hooks -> ChatRequest -> IO LLMResult
genericChat p hooks r = do
  let body = buildBody p False r
  onRequest hooks (providerAdapterName p) body
  result <- try (sendRequest p body)
  case result of
    Left e -> pure $ ResError $ NetworkError (T.pack (show (e :: HttpException)))
    Right (status, respBody) -> do
      onResponse hooks (providerAdapterName p) respBody
      if status == 200
        then parseResponse p respBody
        else pure $ ResError $ HttpError status (T.pack $ show respBody)

-- | Generic object generation via the typeclass.
genericGenerateObject :: (LLMProviderAdapter a) => a -> Hooks -> ChatRequest -> Value -> IO LLMResult
genericGenerateObject p hooks r schema = do
  let body = buildObjectBody p r schema
  onRequest hooks (providerAdapterName p) body
  result <- try (sendObjectRequest p body)
  case result of
    Left e -> pure $ ResError $ NetworkError (T.pack (show (e :: HttpException)))
    Right (status, respBody) -> do
      onResponse hooks (providerAdapterName p) respBody
      if status == 200
        then parseObjectResponse p respBody
        else pure $ ResError $ HttpError status (T.pack $ show respBody)

-- | Generic streaming chat via the typeclass.
genericStreamChat :: (LLMProviderAdapter a) => a -> Hooks -> ChatRequest -> (StreamEvent -> IO ()) -> IO LLMResult
genericStreamChat p hooks r callback = do
  let body = buildBody p True r
  onRequest hooks (providerAdapterName p) body
  result <- try (sendStreamRequest p body callback)
  case result of
    Left e -> pure $ ResError $ NetworkError (T.pack (show (e :: HttpException)))
    Right r' -> do
      case r' of
        ResChat resp -> onResponse hooks (providerAdapterName p) (streamResponseJson resp)
        _ -> pure ()
      pure r'

-- | Convert any LLMProviderAdapter instance into a LLMProvider.
-- Hooks are not baked in — they are passed at call time via 'ChatEnv'.
toProvider :: (LLMProviderAdapter a) => a -> LLMProvider
toProvider p =
  LLMProvider
    { providerName = providerAdapterName p,
      providerChat = genericChat p,
      providerChatStream = genericStreamChat p,
      providerGenerateObject = genericGenerateObject p
    }
