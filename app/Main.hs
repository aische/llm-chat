module Main where

-- import Adapters.StreamChatLoop (streamChatLoopMain)

import Adapters.AgentTask (agentTask)
import Adapters.Repl (repl)
import Adapters.SessionChat (sessionChat)
import AllModels (AllModels (..), getAllModels)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import LLM.Core.LLMProvider (ChatEnv (..), defaultChatEnv)
import LLM.Core.Logger (LogLevel (..), noHooks, withJsonDump, withStderrLogger)
import System.Environment (getEnv)
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
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  userProjectPath <- getEnv "USER_PROJECT_PATH"
  AllModels {gemini_2_5_flash, claude_haiku_4_5, llama_3_2, gpt_4_1, gpt_5_nano} <- getAllModels
  fsConfig <- mkFsConfig userProjectPath
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
      tools =
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
      gpt_5_nanoEnv :: ChatEnv
      gpt_5_nanoEnv =
        (defaultChatEnv gpt_5_nano)
          { envFallbacks = [llama_3_2],
            envHooks = hooks,
            envTools = tools,
            envContextWindow = Just 3,
            envSystem = Just "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to."
          }

      llama_Env :: ChatEnv
      llama_Env =
        (defaultChatEnv llama_3_2)
          { envHooks = hooks,
            envTools = tools,
            envContextWindow = Just 5,
            envSystem = Just "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to."
          }

      gpt_4_1Env :: ChatEnv
      gpt_4_1Env =
        (defaultChatEnv gpt_4_1)
          { envHooks = hooks,
            envTools = tools,
            envContextWindow = Just 3,
            envSystem = Just "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to."
          }

  -- streamChatLoopMain claudeEnv
  -- repl llama_Env
  agentTask llama_Env
