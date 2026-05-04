module AllModels (getAllModels, AllModels (..)) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Control.Retry (fullJitterBackoff, limitRetries)
import Data.Text (Text)
import Data.Text qualified as T
import LLM
  ( ChatEnv,
    Conversation,
    Hooks,
    LogLevel (Debug),
    ModelConfig (..),
    PricingInfo (..),
    StreamEvent (..),
    Usage (..),
    addUsage,
    claudeProvider,
    defaultChatEnv,
    emptyUsage,
    geminiProvider,
    noHooks,
    ollamaProvider,
    withJsonDump,
    withStderrLogger,
  )
import System.Environment (getEnv)

data AllModels = AllModels
  { gemini_2_5_flash :: ModelConfig,
    claude_haiku_4_5 :: ModelConfig,
    llama_3_2 :: ModelConfig
  }

getAllModels :: Hooks -> IO AllModels
getAllModels hooks =
  do
    loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()

    geminiKey <- T.pack <$> getEnv "GEMINI_API_KEY"
    claudeKey <- T.pack <$> getEnv "CLAUDE_API_KEY"

    let gemini = geminiProvider geminiKey
        claude = claudeProvider claudeKey
        gemini_2_5_flash =
          ModelConfig
            { mcGateway = gemini,
              mcModel = "gemini-2.5-flash",
              mcPricing = PricingInfo {pricePerMillionInput = 0.10, pricePerMillionOutput = 0.40},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Just 1_000,
              mcRetry = limitRetries 0 <> fullJitterBackoff 1_000_000
            }
        claude_haiku_4_5 =
          ModelConfig
            { mcGateway = claude,
              mcModel = "claude-haiku-4-5-20251001",
              mcPricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.00},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Nothing,
              mcRetry = limitRetries 3 <> fullJitterBackoff 1_000_000
            }
        llama_3_2 =
          ModelConfig
            { mcGateway = ollamaProvider,
              mcModel = "llama3.2:latest",
              mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Nothing,
              mcRetry = limitRetries 3 <> fullJitterBackoff 1_000_000
            }
    return $ AllModels gemini_2_5_flash claude_haiku_4_5 llama_3_2