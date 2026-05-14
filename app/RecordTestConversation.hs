module RecordTestConversation where

import Adapters.StreamChatLoop (streamChatLoopMain)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.Text qualified as T
import LLM (PricingInfo (..), claudeGateway, geminiGateway, ollamaGateway, openAIGateway)
import LLM.Core.Logger
  ( LogLevel (..),
    noHooks,
    withJsonDump,
    withStderrLogger,
  )
import LLM.Core.Utils (toTool)
import LLM.Generate.Types
  ( ChatEnv (..),
    ModelConfig (..),
  )
import LLM.Generate.Utils (createChatEnv)
import LLM.Tools.Weather (weatherToolTyped)
import System.Environment (getEnv)

testExample :: String -> Bool -> IO ()
testExample name stream = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  model <- getModelConfig name
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
      systemPrompt = "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to, but use only the tools that are available."
      tools =
        [ toTool weatherToolTyped
        ]
      env =
        (createChatEnv model systemPrompt [])
          { envHooks = hooks,
            envTools = tools
          }
  streamChatLoopMain stream env

getModelConfig :: String -> IO ModelConfig
getModelConfig name =
  case name of
    "ollama" ->
      pure $
        ModelConfig
          { mcGateway = ollamaGateway,
            mcModel = "llama3.2:latest",
            mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
            mcMaxTokens = 1024,
            mcTemperature = Nothing,
            mcRequestTimeout = Nothing,
            mcThrottleDelay = Nothing,
            mcRetryCount = 3,
            mcJitterBackoff = 1_000
          }
    "openai" -> do
      apiKey <- T.pack <$> getEnv "OPENAI_API_KEY"
      pure $
        ModelConfig
          { mcGateway = openAIGateway apiKey,
            mcModel = "gpt-4.1-2025-04-14",
            mcPricing = PricingInfo {pricePerMillionInput = 2.0, pricePerMillionOutput = 8.0},
            mcMaxTokens = 1024,
            mcTemperature = Nothing,
            mcRequestTimeout = Nothing,
            mcThrottleDelay = Just 1_000,
            mcRetryCount = 0,
            mcJitterBackoff = 1_000
          }
    "gemini" -> do
      apiKey <- T.pack <$> getEnv "GEMINI_API_KEY"
      pure $
        ModelConfig
          { mcGateway = geminiGateway apiKey,
            mcModel = "gemini-2.5-flash",
            mcPricing = PricingInfo {pricePerMillionInput = 0.10, pricePerMillionOutput = 0.40},
            mcMaxTokens = 1024,
            mcTemperature = Nothing,
            mcRequestTimeout = Nothing,
            mcThrottleDelay = Just 1_000,
            mcRetryCount = 0,
            mcJitterBackoff = 1_000
          }
    "claude" -> do
      apiKLey <- T.pack <$> getEnv "CLAUDE_API_KEY"
      pure $
        ModelConfig
          { mcGateway = claudeGateway apiKLey,
            mcModel = "claude-haiku-4-5-20251001",
            mcPricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.00},
            mcMaxTokens = 1024,
            mcTemperature = Nothing,
            mcRequestTimeout = Nothing,
            mcThrottleDelay = Nothing,
            mcRetryCount = 3,
            mcJitterBackoff = 1_000
          }
    _ -> error "unknown model name. name should be one of:\n ollama\n claude\n gemini\n openai"
