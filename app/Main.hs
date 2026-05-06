module Main where

import Adapters.Repl (repl)
import Adapters.SessionChat (sessionChat)
import AllModels (AllModels (..), getAllModels)
import Autodocodec qualified as AC
import Autodocodec.Aeson (encodeJSONViaCodec)
import Autodocodec.Schema (jsonSchemaVia)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Control.Monad (forM, forM_)
import Data.Aeson (FromJSON, ToJSON (toJSON), Value, encode, fromJSON, object, (.=))
import Data.Aeson qualified as AE
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM (LLMProvider (providerName), ModelConfig (mcModel), Usage, createChatEnv, toTool)
import LLM.Core.Chat (generateObject, generateObject', runChat)
import LLM.Core.LLMProvider (ChatEnv (..), defaultChatEnv)
import LLM.Core.Logger (LogLevel (..), noHooks, withJsonDump, withStderrLogger)
import LLM.Core.ProviderUtils (normalizeSchemaOpenAI)
import LLM.Core.Types (Conversation (Conversation), LLMError (ParseError), LLMObjectResult, LLMRes (ResError, ResOk))
import LLM.Core.Utils (emptyConversation, printValue)
import System.Environment (getEnv)
import Tools.FsConfig (mkFsConfig)
import Tools.History (historyToolTyped)
import Tools.Readdir (readdirToolTyped)
import Tools.Readfile (readfileToolTyped)
import Tools.ReplaceInFile (replaceInFileToolTyped)
import Tools.Writefile (writefileToolTyped)

main :: IO ()
main = do
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
      claudeEnv =
        (createChatEnv claude_haiku_4_5 systemPrompt tools)
          { envHooks = hooks,
            envContextWindow = Just 3
          }
      gpt5NanoEnv =
        (createChatEnv gpt_5_nano systemPrompt tools)
          { envHooks = hooks,
            envContextWindow = Just 3
          }
      llamaEnv =
        (createChatEnv llama_3_2 systemPrompt tools)
          { envHooks = hooks,
            envContextWindow = Just 5
          }
      gpt41Env =
        (createChatEnv gpt_4_1 systemPrompt tools)
          { envHooks = hooks,
            envContextWindow = Just 3
          }
      gemini25flashEnv =
        (createChatEnv gemini_2_5_flash systemPrompt tools)
          { envHooks = hooks,
            envContextWindow = Just 3
          }
  repl gpt41Env
