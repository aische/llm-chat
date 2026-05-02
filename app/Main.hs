module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM
import System.Environment (getEnv)
import Tools.Age (ageTool)
import Tools.Weather (weatherTool)

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()

  geminiKey <- T.pack <$> getEnv "GEMINI_API_KEY"
  claudeKey <- T.pack <$> getEnv "CLAUDE_API_KEY"

  let gemini = geminiClient geminiKey
      claude = claudeClient claudeKey
      tools = [weatherTool, ageTool]

  putStrLn "=== Gemini ==="
  geminiConv <- demo gemini (defaultChatConfig "gemini-2.5-flash") tools []
  -- Continue the conversation with the returned history
  _ <- demo gemini (defaultChatConfig "gemini-2.5-flash") tools geminiConv

  putStrLn "\n=== Claude ==="
  claudeConv <- demo claude (defaultChatConfig "claude-haiku-4-5-20251001") tools []
  _ <- demo claude (defaultChatConfig "claude-haiku-4-5-20251001") tools claudeConv

  pure ()

-- | Run a single user turn through runChat, print results, return the conversation
demo :: LLMClient -> ChatConfig -> [Tool] -> Conversation -> IO Conversation
demo client cfg tools conv = do
  let msg =
        if null conv
          then "How old is Alice? And what's the weather like in London?"
          else "Thanks! And in Paris?"
  putStrLn $ "> " <> T.unpack msg
  result <- runChat client cfg tools conv msg
  case result of
    Left err -> do
      putStrLn $ "Error: " <> show err
      pure conv
    Right (text, conv') -> do
      TIO.putStrLn text
      putStrLn $ "  (" <> show (length conv') <> " turns in history)"
      pure conv'
