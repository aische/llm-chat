{-# OPTIONS_GHC -Wall #-}

module CreateEnv
  ( createDefaultEnv,
  )
where

import AllModels (AllModels (..), getAllModels)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import LLM.Core.Logger
  ( LogLevel (..),
    noHooks,
    withJsonDump,
    withStderrLogger,
  )
import LLM.Generate.Types (ChatEnv (..))
import LLM.Core.Utils (toTool)
import LLM.Generate.Utils (createChatEnv)
import LLM.Tools.FsConfig (mkFsConfig)
import LLM.Tools.History (historyToolTyped)
import LLM.Tools.Readdir (readdirToolTyped)
import LLM.Tools.Readfile (readfileToolTyped)
import LLM.Tools.ReplaceInFile (replaceInFileToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)
import System.Environment (getEnv)

createDefaultEnv :: IO ChatEnv
createDefaultEnv = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  userProjectPath <- getEnv "USER_PROJECT_PATH"
  AllModels {gemini_2_5_flash, claude_haiku_4_5, llama_3_2, gpt_4_1, gpt_5_nano} <- getAllModels
  fsConfig <- mkFsConfig userProjectPath
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
      tools =
        [ toTool historyToolTyped,
          toTool $ readfileToolTyped fsConfig,
          toTool $ writefileToolTyped fsConfig,
          toTool $ replaceInFileToolTyped fsConfig,
          toTool $ readdirToolTyped fsConfig
        ]
      systemPrompt = "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to."
      _claudeEnv =
        (createChatEnv claude_haiku_4_5 systemPrompt tools)
          { envHooks = hooks,
            envContextWindow = Just 3
          }
      _gpt5NanoEnv =
        (createChatEnv gpt_5_nano systemPrompt tools)
          { envHooks = hooks,
            envContextWindow = Just 3
          }
      _llamaEnv =
        (createChatEnv llama_3_2 systemPrompt tools)
          { envHooks = hooks,
            envContextWindow = Just 5
          }
      _gpt41Env =
        (createChatEnv gpt_4_1 systemPrompt tools)
          { envHooks = hooks,
            envContextWindow = Just 3
          }
      _gemini25flashEnv =
        (createChatEnv gemini_2_5_flash systemPrompt tools)
          { envHooks = hooks,
            envContextWindow = Just 3
          }
  pure _llamaEnv
