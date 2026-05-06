module AllModels (getAllModels, AllModels (..)) where

import Control.Retry (fullJitterBackoff, limitRetries)
import Data.Text (Text)
import Data.Text qualified as T
import LLM
  ( ChatEnv,
    Conversation,
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
    ollamaProvider,
    openAIProvider,
    withJsonDump,
    withStderrLogger,
  )
import System.Environment (getEnv)

data AllModels = AllModels
  { gemini_2_5_flash :: ModelConfig,
    claude_haiku_4_5 :: ModelConfig,
    llama_3_2 :: ModelConfig,
    gpt_4_1 :: ModelConfig,
    gpt_5_nano :: ModelConfig,
    gpt_5_4_nano :: ModelConfig
  }

getAllModels :: IO AllModels
getAllModels =
  do
    geminiKey <- T.pack <$> getEnv "GEMINI_API_KEY"
    claudeKey <- T.pack <$> getEnv "CLAUDE_API_KEY"
    openAIKey <- T.pack <$> getEnv "OPENAI_API_KEY"

    let gemini = geminiProvider geminiKey
        claude = claudeProvider claudeKey
        openAI = openAIProvider openAIKey
        gpt_5_nano =
          ModelConfig
            { mcProvider = openAI,
              -- mcModel = "gpt-5.5",
              mcModel = "gpt-5-nano-2025-08-07",
              mcPricing = PricingInfo {pricePerMillionInput = 0.05, pricePerMillionOutput = 0.4},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Nothing,
              mcRetry = limitRetries 0 <> fullJitterBackoff 1_000_000
            }
        gpt_5_4_nano =
          ModelConfig
            { mcProvider = openAI,
              mcModel = "gpt-5.4-nano-2026-03-17",
              mcPricing = PricingInfo {pricePerMillionInput = 0.2, pricePerMillionOutput = 1.25},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Nothing,
              mcRetry = limitRetries 0 <> fullJitterBackoff 1_000_000
            }
        gpt_4_1 =
          ModelConfig
            { mcProvider = openAI,
              mcModel = "gpt-4.1-2025-04-14",
              mcPricing = PricingInfo {pricePerMillionInput = 2.0, pricePerMillionOutput = 8.0},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Just 1_000,
              mcRetry = limitRetries 0 <> fullJitterBackoff 1_000_000
            }
        gemini_2_5_flash =
          ModelConfig
            { mcProvider = gemini,
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
            { mcProvider = claude,
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
            { mcProvider = ollamaProvider,
              mcModel = "llama3.2:latest",
              mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Nothing,
              mcRetry = limitRetries 3 <> fullJitterBackoff 1_000_000
            }
    return $ AllModels {gemini_2_5_flash = gemini_2_5_flash, claude_haiku_4_5 = claude_haiku_4_5, llama_3_2 = llama_3_2, gpt_4_1 = gpt_4_1, gpt_5_nano = gpt_5_nano, gpt_5_4_nano = gpt_5_4_nano}
