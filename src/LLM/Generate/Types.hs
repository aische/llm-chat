module LLM.Generate.Types (ModelConfig (..), ChatEnv (..), Generatable, GeneratedResult) where

import Autodocodec (HasCodec)
import Control.Retry (RetryPolicyM)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import LLM.Core.Abort (AbortSignal)
import LLM.Core.Logger (Hooks)
import LLM.Core.Types (Conversation, LLMError, LLMGateway (..), Tool)
import LLM.Core.Usage (PricingInfo (..), Usage)

-- | Infrastructure-level configuration for a specific model.
-- Bundles together everything needed to reach one model endpoint.
-- Use a list of these in 'ChatEnv' for fallback across models/providers.
data ModelConfig = ModelConfig
  { mcGateway :: LLMGateway,
    mcModel :: Text,
    mcPricing :: PricingInfo,
    mcMaxTokens :: Int,
    mcTemperature :: Maybe Double,
    mcRequestTimeout :: Maybe Int, -- milliseconds; timeout the whole request if it takes too long
    mcThrottleDelay :: Maybe Int, -- milliseconds; wait before each API call
    -- mcRetry :: RetryPolicyM IO,
    mcRetryCount :: Int,
    mcJitterBackoff :: Int -- milliseconds; wait before each retry
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
    envHooks :: Hooks,
    envAbortSignal :: Maybe AbortSignal
  }

type Generatable t = (FromJSON t, HasCodec t)

type GeneratedResult a = Either (LLMError, Conversation, Usage) a
