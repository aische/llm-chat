module LLM.Core.LLMProvider
  ( LLMProvider (..),
  )
where

import Data.Aeson (Value)
import Data.Text (Text)
import LLM.Core.Logger (Hooks)
import LLM.Core.Types
  ( ChatRequest,
    LLMObjectResult,
    LLMTextResult,
    StreamEvent,
  )

-- | A provider-agnostic gateway for making LLM API calls.
-- This is the runtime representation — any 'LLMProviderAdapter' can be
-- converted into a 'LLMProvider' via 'toProvider'.
--
-- LLMProvider functions receive 'Hooks' at call time so the same gateway
-- can be shared across chat sessions with different hook configurations.
data LLMProvider = LLMProvider
  { providerName :: Text,
    providerGenerateText :: Hooks -> ChatRequest -> IO LLMTextResult,
    providerStreamText :: Hooks -> ChatRequest -> (StreamEvent -> IO ()) -> IO LLMTextResult,
    providerGenerateObject :: Hooks -> Value -> ChatRequest -> IO LLMObjectResult
  }
