module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.IORef
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM
import System.Environment (getEnv)
import Text.Printf (printf)
import Tools.Age (ageTool)
import Tools.Weather (weatherTool)

prompts =
  [ "how old is alice?",
    "how's the weather in london?",
    "and in paris?"
  ]

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()

  geminiKey <- T.pack <$> getEnv "GEMINI_API_KEY"
  claudeKey <- T.pack <$> getEnv "CLAUDE_API_KEY"

  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
      gemini = geminiClient noHooks geminiKey
      claude = claudeClient hooks claudeKey
      tools = [weatherTool, ageTool]

  let geminiPricing = PricingInfo {pricePerMillionInput = 0.10, pricePerMillionOutput = 0.40}
      geminiConfig = (defaultChatConfig "gemini-2.0-flash") {cfgHooks = hooks}
      claudePricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.00}
      claudeConfig =
        (defaultChatConfig "claude-haiku-4-5-20251001")
          { cfgHooks = hooks
          }

  -- putStrLn "=== Gemini ==="
  -- _ <- streamChatLoop gemini geminiConfig tools geminiPricing prompts

  putStrLn "\n=== Claude ==="
  _ <- streamChatLoop claude claudeConfig tools claudePricing prompts
  pure ()
