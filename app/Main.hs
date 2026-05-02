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
      msgs = [user "What's the weather like in London right now? And how old is Alice?"]

  putStrLn "=== Gemini (with tools) ==="
  runWithTools gemini "gemini-2.5-flash" tools msgs

  putStrLn "\n=== Claude (with tools) ==="
  runWithTools claude "claude-haiku-4-5-20251001" tools msgs

-- | Send a request with tools, execute any tool calls, and print the final answer
runWithTools :: LLMClient -> T.Text -> [Tool] -> [Message] -> IO ()
runWithTools client model tools msgs = do
  let req0 =
        (defaultRequest model msgs)
          { reqTools = map toolDef tools
          }

  -- Step 1: send the initial request
  result1 <- clientChat client req0
  case result1 of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp1
      | hasToolCalls resp1 -> do
          let calls = getToolCalls resp1
          putStrLn $ "Model requested " <> show (length calls) <> " tool call(s):"
          mapM_ (\tc -> TIO.putStrLn $ "  " <> tcName tc <> " " <> T.pack (show (tcArguments tc))) calls

          -- Step 2: execute the tools
          results <- executeTools tools calls

          -- Step 3: send tool results back
          let req1 =
                req0
                  { reqPendingToolCalls = calls,
                    reqToolResults = results
                  }
          result2 <- clientChat client req1
          case result2 of
            Left err -> putStrLn $ "Error: " <> show err
            Right resp2 -> do
              putStrLn "Final response:"
              TIO.putStrLn $ respText resp2
      | otherwise -> do
          putStrLn "Model responded directly (no tool call):"
          TIO.putStrLn $ respText resp1
