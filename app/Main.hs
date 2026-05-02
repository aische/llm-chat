module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.Aeson (Value, object, (.=))
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

  putStrLn "=== Gemini (with tools) ==="
  runWithTools gemini "gemini-2.5-flash"

  putStrLn "\n=== Claude (with tools) ==="
  runWithTools claude "claude-haiku-4-5-20251001"

-- | Run a single tool-calling round trip
runWithTools :: LLMClient -> T.Text -> IO ()
runWithTools client model = do
  let msgs = [user "What's the weather like in London right now?"]
      req0 =
        (defaultRequest model msgs)
          { reqTools = [weatherTool]
          }

  -- Step 1: send the initial request
  result1 <- clientChat client req0
  case result1 of
    Left err -> putStrLn $ "Error: " <> show err
    Right resp1
      | hasToolCalls resp1 -> do
          let calls = [tc | ToolCallBlock tc <- respContent resp1]
          putStrLn $ "Model requested " <> show (length calls) <> " tool call(s):"
          mapM_ (\tc -> TIO.putStrLn $ "  " <> tcName tc <> " " <> T.pack (show (tcArguments tc))) calls

          -- Step 2: "execute" the tools (dummy results)
          let results = map executeWeatherTool calls
              req1 =
                req0
                  { reqPendingToolCalls = calls,
                    reqToolResults = results
                  }

          -- Step 3: send tool results back
          result2 <- clientChat client req1
          case result2 of
            Left err -> putStrLn $ "Error: " <> show err
            Right resp2 -> do
              putStrLn "Final response:"
              TIO.putStrLn $ respText resp2
      | otherwise -> do
          putStrLn "Model responded directly (no tool call):"
          TIO.putStrLn $ respText resp1

-- | A dummy weather tool definition
weatherTool :: ToolDef
weatherTool =
  ToolDef
    { toolName = "get_weather",
      toolDescription = "Get the current weather for a given location",
      toolParameters = weatherSchema
    }

weatherSchema :: Value
weatherSchema =
  object
    [ "type" .= ("object" :: T.Text),
      "properties"
        .= object
          [ "location"
              .= object
                [ "type" .= ("string" :: T.Text),
                  "description" .= ("City name, e.g. London" :: T.Text)
                ]
          ],
      "required" .= (["location"] :: [T.Text])
    ]

-- | Dummy tool execution — always returns the same weather
executeWeatherTool :: ToolCall -> ToolResult
executeWeatherTool tc =
  toolResult tc "Partly cloudy, 18°C, light breeze from the west."
