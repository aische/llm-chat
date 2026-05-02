module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM
import System.Environment (getEnv)
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

  putStrLn "=== Gemini ==="
  _ <- conversationLoop gemini (defaultChatConfig "gemini-2.0-flash") tools prompts

  putStrLn "\n=== Claude ==="
  _ <- conversationLoop claude (defaultChatConfig "claude-haiku-4-5-20251001") tools prompts
  pure ()

conversationLoop :: LLMClient -> ChatConfig -> [Tool] -> [T.Text] -> IO Conversation
conversationLoop client cfg tools prompts = aux [] prompts
  where
    aux conv [] = return conv
    aux conv (prompt : prompts) = do
      putStrLn $ "> " <> T.unpack prompt
      result <- runChat client cfg tools conv prompt
      case result of
        Left err -> do
          putStrLn $ "Error: " <> show err
          pure conv
        Right (text, conv') -> do
          TIO.putStrLn text
          putStrLn $ "  (" <> show (length conv') <> " turns in history)"
          aux conv' prompts
