{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Adapters.Repl (repl)
import Adapters.SessionChat (SessionCommand (ClearSession, PromptSession, ShowSession), sessionChat)
import AllModels (AllModels (..), getAllModels)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import CreateEnv (createDefaultEnv)
import Data.Text qualified as T
import Example (example)
import Options.Applicative
import TestExample (testExample)

main :: IO ()
main = mainInternal =<< execParser opts
  where
    opts =
      info
        (runtimeArgsParser <**> helper)
        ( fullDesc
            <> progDesc "simple terminal based llm chat"
            <> header "\n"
        )

mainInternal :: RuntimeArgs -> IO ()
mainInternal args = do
  case args of
    ReplArgs -> createDefaultEnv >>= repl
    (TestRecorderArgs name stream) -> do
      print name
      print stream
      testExample name stream
    SessionClear -> createDefaultEnv >>= \env -> sessionChat env ClearSession
    SessionShow -> createDefaultEnv >>= \env -> sessionChat env ShowSession
    (SessionPrompt p) -> createDefaultEnv >>= \env -> sessionChat env (PromptSession (T.pack p))

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
