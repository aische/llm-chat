module LLM.Generate.Types (ModelConfig (..), ChatEnv (..)) where

import Control.Retry (RetryPolicyM)
import Data.Text (Text)
import LLM.Core.Abort (AbortSignal)
import LLM.Core.Logger (Hooks)
import LLM.Core.Types (LLMGateway (..), Tool)
import LLM.Core.Usage (PricingInfo (..))

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
    envHooks :: Hooks,
    envAbortSignal :: Maybe AbortSignal
  }
