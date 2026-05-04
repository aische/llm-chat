module LLM.Core.LLMProvider
  ( LLMProvider (..),
    ModelConfig (..),
    ChatEnv (..),
    defaultChatEnv,
  )
where

import Control.Retry (RetryPolicyM)
import Data.Text (Text)
import LLM.Core.Logger (Hooks, noHooks)
import LLM.Core.Types
  ( ChatRequest,
    LLMResult,
    StreamEvent,
    Tool,
  )
import LLM.Core.Usage (PricingInfo (..))

-- | A provider-agnostic gateway for making LLM API calls.
-- This is the runtime representation — any 'LLMProviderAdapter' can be
-- converted into a 'LLMProvider' via 'toProvider'.
--
-- LLMProvider functions receive 'Hooks' at call time so the same gateway
-- can be shared across chat sessions with different hook configurations.
data LLMProvider = LLMProvider
  { providerName :: Text,
    providerChat :: Hooks -> ChatRequest -> IO LLMResult,
    providerChatStream :: Hooks -> ChatRequest -> (StreamEvent -> IO ()) -> IO LLMResult
  }

-- | Infrastructure-level configuration for a specific model.
-- Bundles together everything needed to reach one model endpoint.
-- Use a list of these in 'ChatEnv' for fallback across models/providers.
data ModelConfig = ModelConfig
  { mcGateway :: LLMProvider,
    mcModel :: Text,
    mcPricing :: PricingInfo,
    mcMaxTokens :: Int,
    mcTemperature :: Maybe Double,
    mcRequestTimeout :: Maybe Int, -- milliseconds; timeout the whole request if it takes too long
    mcThrottleDelay :: Maybe Int, -- milliseconds; wait before each API call
    mcRetry :: RetryPolicyM IO
  }

-- | Application-level chat configuration.
-- Tools, system prompt, and hooks stay fixed across fallback attempts.
data ChatEnv = ChatEnv
  { envModel :: ModelConfig, -- primary
    envFallbacks :: [ModelConfig], -- fallbacks, tried in order
    envSystem :: Maybe Text,
    envTools :: [Tool],
    envMaxToolRounds :: Int,
    envContextWindow :: Maybe Int, -- max recent turns sent to the model; Nothing = all
    envHooks :: Hooks
  }

-- | Sensible defaults — single model, no fallback.
defaultChatEnv :: ModelConfig -> ChatEnv
defaultChatEnv mc =
  ChatEnv
    { envModel = mc,
      envFallbacks = [],
      envSystem = Nothing,
      envTools = [],
      envMaxToolRounds = 10,
      envContextWindow = Nothing,
      envHooks = noHooks
    }
