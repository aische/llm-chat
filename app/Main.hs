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

-- main :: IO ()
-- main = main1

main :: IO ()
main = main0 =<< execParser opts
  where
    opts =
      info
        (runtimeArgsParser <**> helper)
        ( fullDesc
            <> progDesc "blabla"
            <> header "blub"
        )

main0 :: RuntimeArgs -> IO ()
main0 args = do
  case args of
    ReplArgs -> createDefaultEnv >>= repl
    (TestRecorderArgs name stream) -> do
      print name
      print stream
      testExample name stream
    (SessionClear sid) -> createDefaultEnv >>= \env -> sessionChat env ClearSession
    (SessionShow sid) -> createDefaultEnv >>= \env -> sessionChat env ShowSession
    (SessionPrompt sid p) -> createDefaultEnv >>= \env -> sessionChat env (PromptSession (T.pack p))

runtimeArgsParser :: Parser RuntimeArgs
runtimeArgsParser = testRecorderArgs <|> replArgs <|> sessionClear <|> sessionPrompt <|> sessionShow

data RuntimeArgs
  = TestRecorderArgs
      { name :: String,
        stream :: Bool
      }
  | ReplArgs
  | SessionClear
      { sid :: String
      }
  | SessionShow
      { sid :: String
      }
  | SessionPrompt
      { sid :: String,
        prompt :: String
      }

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

replArgs :: Parser RuntimeArgs
replArgs =
  flag'
    ReplArgs
    ( long "repl"
        <> help "Run chat repl"
    )

sessionClear :: Parser RuntimeArgs
sessionClear =
  SessionClear
    <$> strOption
      ( long "clear-session"
          <> metavar "SESSIONID"
          <> help "run session"
      )

sessionShow :: Parser RuntimeArgs
sessionShow =
  SessionShow
    <$> strOption
      ( long "show-session"
          <> metavar "SESSIONID"
          <> help "run session"
      )

sessionPrompt :: Parser RuntimeArgs
sessionPrompt =
  SessionPrompt
    <$> strOption
      ( long "prompt-session"
          <> metavar "SESSIONID"
          <> help "run session"
      )
    <*> argument str (metavar "PROMPT")
