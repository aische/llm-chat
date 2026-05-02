module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
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

  let gemini = geminiClient geminiKey
      claude = claudeClient claudeKey
      tools = [weatherTool, ageTool]

  let geminiPricing = PricingInfo {pricePerMillionInput = 0.10, pricePerMillionOutput = 0.40}
  let claudePricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.00}

  -- putStrLn "=== Gemini ==="
  -- _ <- conversationLoop gemini (defaultChatConfig "gemini-2.0-flash") tools geminiPricing prompts

  putStrLn "\n=== Claude ==="
  _ <- conversationLoop claude (defaultChatConfig "claude-haiku-4-5-20251001") tools claudePricing prompts
  pure ()

conversationLoop :: LLMClient -> ChatConfig -> [Tool] -> PricingInfo -> [T.Text] -> IO Conversation
conversationLoop client cfg tools pricing = aux emptyUsage []
  where
    aux totalUsage conv [] = do
      putStrLn $
        "\n  Total: "
          <> show (usageInputTokens totalUsage)
          <> " input + "
          <> show (usageOutputTokens totalUsage)
          <> " output tokens"
      printf "  Estimated cost: $%.6f\n" (estimateCost pricing totalUsage)
      return conv
    aux totalUsage conv (prompt : rest) = do
      putStrLn $ "> " <> T.unpack prompt
      result <- runChat client cfg tools conv prompt
      case result of
        Left err -> do
          putStrLn $ "Error: " <> show err
          pure conv
        Right (text, conv', usage) -> do
          TIO.putStrLn text
          putStrLn $
            "  ("
              <> show (length conv')
              <> " turns, "
              <> show (usageInputTokens usage)
              <> " in + "
              <> show (usageOutputTokens usage)
              <> " out tokens)"
          aux (addUsage totalUsage usage) conv' rest
