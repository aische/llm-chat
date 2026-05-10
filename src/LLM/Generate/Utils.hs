module LLM.Generate.Utils
  ( defaultChatEnv,
    createChatEnv,
    createModelConfig,
  )
where

import Control.Retry (fullJitterBackoff, limitRetries)
import Data.Text (Text)
import LLM.Core.Logger (noHooks)
import LLM.Core.Types (LLMGateway, Tool)
import LLM.Core.Usage (PricingInfo (..))
import LLM.Generate.Types (ChatEnv (..), ModelConfig (..))

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
      envHooks = noHooks,
      envAbortSignal = Nothing
    }

createChatEnv :: ModelConfig -> Text -> [Tool] -> ChatEnv
createChatEnv mc system tools =
  ChatEnv
    { envModel = mc,
      envFallbacks = [],
      envSystem = Just system,
      envTools = tools,
      envMaxToolRounds = 10,
      envContextWindow = Nothing,
      envHooks = noHooks,
      envAbortSignal = Nothing
    }

createModelConfig :: LLMGateway -> Text -> ModelConfig
createModelConfig gateway modelName =
  ModelConfig
    { mcGateway = gateway,
      mcModel = modelName,
      mcPricing = PricingInfo {pricePerMillionInput = 0, pricePerMillionOutput = 0},
      mcMaxTokens = 1024,
      mcTemperature = Nothing,
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetry = limitRetries 0 <> fullJitterBackoff 1_000_000
    }
