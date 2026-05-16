module Example2 where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Core.Logger
    ( LogLevel(Debug), noHooks, withJsonDump, withStderrLogger )
import LLM.Core.Usage ( PricingInfo(..) )
import LLM.Core.Utils ( emptyConversation, toTool )
import LLM.Generate.Types
    ( ChatEnv(envFallbacks, envHooks, envTools), ModelConfig(..) )
import LLM.Generate.Utils ( createChatEnv )
import LLM.Providers.Claude ( claudeGateway )
import LLM.Providers.OpenAI ( openAIGateway )
import LLM.Generate.Generate (generateText)
import LLM.Tools.FsConfig (mkFsConfig)
import LLM.Tools.Readdir (readdirToolTyped)
import LLM.Tools.Readfile (readfileToolTyped)
import LLM.Tools.ReplaceInFile (replaceInFileToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)
import System.Environment (getEnv)

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  claudeKey <- T.pack <$> getEnv "CLAUDE_API_KEY"
  openAIKey <- T.pack <$> getEnv "OPENAI_API_KEY"
  userProjectPath <- getEnv "USER_PROJECT_PATH"
  fsConfig <- mkFsConfig userProjectPath
  let claude = claudeGateway claudeKey
      openAI = openAIGateway openAIKey
      gpt_4_1 =
        ModelConfig
          { mcGateway = openAI,
            mcModel = "gpt-4.1-2025-04-14",
            mcPricing = PricingInfo {pricePerMillionInput = 2.0, pricePerMillionOutput = 8.0},
            mcMaxTokens = 1024,
            mcTemperature = Nothing,
            mcRequestTimeout = Nothing,
            mcThrottleDelay = Just 1_000,
            mcRetryCount = 0,
            mcJitterBackoff = 1_000
          }
      claude_haiku_4_5 =
        ModelConfig
          { mcGateway = claude,
            mcModel = "claude-haiku-4-5-20251001",
            mcPricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.00},
            mcMaxTokens = 1024,
            mcTemperature = Nothing,
            mcRequestTimeout = Nothing,
            mcThrottleDelay = Nothing,
            mcRetryCount = 0,
            mcJitterBackoff = 1_000
          }
      hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
      env =
        (createChatEnv gpt_4_1 "you are a friendly assistant." [])
          { envFallbacks = [claude_haiku_4_5],
            envTools =
              [ toTool $ readfileToolTyped fsConfig,
                toTool $ writefileToolTyped fsConfig,
                toTool $ replaceInFileToolTyped fsConfig,
                toTool $ readdirToolTyped fsConfig
              ],
            envHooks = hooks
          }
  result <- generateText env emptyConversation "create a document 'paris.md' with a poem about paris"
  case result of
    Left e -> print e
    Right (answer, _, _) -> TIO.putStrLn answer
