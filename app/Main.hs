module Main where

import Adapters.Repl (repl)
-- import Adapters.StreamChatLoop (streamChatLoopMain)
import AllModels (AllModels (..), getAllModels)
import LLM.Core.LLMProvider (ChatEnv (..), defaultChatEnv)
import LLM.Core.Logger (LogLevel (..), noHooks, withJsonDump, withStderrLogger)
import Tools.Age (ageTool)
import Tools.FsConfig (mkFsConfig)
import Tools.History (historyTool)
import Tools.Readdir (readdirTool)
import Tools.Readfile (readfileTool)
import Tools.ReplaceInFile (replaceInFileTool)
import Tools.Weather (weatherTool)
import Tools.Writefile (writefileTool)

main :: IO ()
main = do
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
  AllModels {gemini_2_5_flash, claude_haiku_4_5, llama_3_2} <- getAllModels hooks
  fsConfig <- mkFsConfig "/Users/daniel/Desktop/hask-llm-data"
  let tools =
        [ weatherTool,
          ageTool,
          historyTool,
          readfileTool fsConfig,
          writefileTool fsConfig,
          replaceInFileTool fsConfig,
          readdirTool fsConfig
        ]
      claudeEnv :: ChatEnv
      claudeEnv =
        (defaultChatEnv claude_haiku_4_5)
          { envFallbacks = [gemini_2_5_flash, llama_3_2],
            envHooks = hooks,
            envTools = tools,
            envContextWindow = Just 3,
            envSystem = Just "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to."
          }

  -- streamChatLoopMain claudeEnv
  repl claudeEnv
