module Main where

import Adapters.Repl (repl)
import Adapters.SessionChat (SessionCommand (ClearSession, PromptSession, ShowSession), sessionChat)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
-- import CreateEnv (createDefaultEnv)
import Data.Map qualified as Map
import Data.Text qualified as T
import Example (example)
import LLM (LogLevel (Debug), noHooks, withJsonDump, withStderrLogger)
import LLM.Generate.LoadModels (loadEnvs)
import LLM.Generate.Types (ChatEnv (envHooks))
import Options.Applicative
import TestExample (testExample)

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  mainInternal =<< execParser opts
  where
    opts =
      info
        (runtimeArgsParser <**> helper)
        ( fullDesc
            <> progDesc "simple terminal based llm chat"
            <> header "\n"
        )

createDefaultEnv :: IO ChatEnv
createDefaultEnv = do
  (chatEnvs, modelConfigs, gateways) <- either error id <$> loadEnvs
  print $ Map.keys chatEnvs
  let env = chatEnvs Map.! "default"
  pure env {envHooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks}

mainInternal :: RuntimeArgs -> IO ()
mainInternal args = do
  env <- createDefaultEnv
  case args of
    ReplArgs -> repl env
    (TestRecorderArgs name stream) -> do
      print name
      print stream
      testExample name stream
    SessionClear -> sessionChat env ClearSession
    SessionShow -> sessionChat env ShowSession
    SessionPrompt p -> sessionChat env (PromptSession (T.pack p))

data RuntimeArgs
  = TestRecorderArgs
      { name :: String,
        stream :: Bool
      }
  | ReplArgs
  | SessionClear
  | SessionShow
  | SessionPrompt
      { prompt :: String
      }

runtimeArgsParser :: Parser RuntimeArgs
runtimeArgsParser =
  hsubparser
    ( command "test-recorder" (info testRecorderArgs (progDesc "Start the test recorder"))
        <> command "repl" (info (pure ReplArgs) (progDesc "Start the REPL (not saved in session)"))
        <> command "clear" (info (pure SessionClear) (progDesc "Clear the session"))
        <> command "show" (info (pure SessionShow) (progDesc "Show session history"))
        <> command "prompt" (info sessionPrompt (progDesc "Interactive session prompt (saved in session)"))
    )

testRecorderArgs :: Parser RuntimeArgs
testRecorderArgs =
  TestRecorderArgs
    <$> strOption
      ( long "test"
          <> metavar "MODELNAME"
          <> help "run test conversation"
      )
    <*> switch
      ( long "stream"
          <> short 's'
          <> help "Whether to use streaming"
      )

sessionPrompt :: Parser RuntimeArgs
sessionPrompt =
  SessionPrompt
    <$> argument str (metavar "PROMPT")
