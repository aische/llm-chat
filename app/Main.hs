module Main where

import Adapters.Repl (repl)
import Adapters.SessionChat (SessionCommand (ClearSession, PromptSession, ShowSession), sessionChat)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.Text qualified as T
import Example1 qualified as E1
import Example2 qualified as E2
import Example3 qualified as E3
import LLM.Core.Logger
  ( LogLevel (Debug),
    noHooks,
    withJsonDump,
    withStderrLogger,
  )
import LLM.Generate.LoadModels (getLoadedEnv, getLoadedEnvs, loadEnvs)
import LLM.Generate.Types (ChatEnv (..))
import Options.Applicative
import RecordTestConversation (testExample)

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
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
  envs <- either error id <$> loadEnvs
  case getLoadedEnv envs hooks "default" of
    Left err -> error err
    Right env -> pure env

createDefaultEnvs :: IO (ChatEnv, ChatEnv)
createDefaultEnvs = do
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
  envs <- either error id <$> loadEnvs
  case getLoadedEnvs envs hooks ("default", "default-readonly") of
    Left err -> error err
    Right env -> pure env

mainInternal :: RuntimeArgs -> IO ()
mainInternal args = do
  (env, _env2) <- createDefaultEnvs
  case args of
    ReplArgs -> repl env
    (TestRecorderArgs name stream) -> do
      print name
      print stream
      testExample name stream
    SessionClear -> sessionChat env ClearSession
    SessionShow -> sessionChat env ShowSession
    SessionPrompt p -> sessionChat env (PromptSession (T.pack p))
    Example1 -> E1.main
    Example2 -> E2.main
    Example3 -> E3.main

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
  | Example1
  | Example2
  | Example3

runtimeArgsParser :: Parser RuntimeArgs
runtimeArgsParser =
  hsubparser
    ( command "record-test-conversation" (info testRecorderArgs (progDesc "Start the test recorder"))
        <> command "repl" (info (pure ReplArgs) (progDesc "Start the REPL (not saved in session)"))
        <> command "clear" (info (pure SessionClear) (progDesc "Clear the session"))
        <> command "show" (info (pure SessionShow) (progDesc "Show session history"))
        <> command "prompt" (info sessionPrompt (progDesc "Interactive session prompt (saved in session)"))
        <> command "example1" (info (pure Example1) (progDesc "Example1 from Readme.md"))
        <> command "example2" (info (pure Example2) (progDesc "Example2 from Readme.md"))
        <> command "example3" (info (pure Example3) (progDesc "Example3 from Readme.md (generateObject)"))
    )

testRecorderArgs :: Parser RuntimeArgs
testRecorderArgs =
  TestRecorderArgs
    <$> strOption
      ( long "test"
          <> metavar "PROVIDERNAME"
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
