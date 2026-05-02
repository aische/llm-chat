module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM
import System.Environment (getEnv)

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()

  geminiKey <- T.pack <$> getEnv "GEMINI_API_KEY"
  claudeKey <- T.pack <$> getEnv "CLAUDE_API_KEY"

  let gemini = geminiClient geminiKey
      claude = claudeClient claudeKey
      msgs = [user "What is 2 + 2? One word only."]

  putStrLn "=== Gemini ==="
  clientChat gemini (defaultRequest "gemini-2.5-flash" msgs) >>= report

  putStrLn "\n=== Claude ==="
  clientChat claude (defaultRequest "claude-haiku-4-5-20251001" msgs) >>= report

report :: LLMResult -> IO ()
report (Left err) = putStrLn $ "Error: " <> show err
report (Right resp) = TIO.putStrLn (respText resp)
