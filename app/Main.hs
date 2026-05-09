{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import AllModels (AllModels (..), getAllModels)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Example (example)
import Main1 (main1)
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
main0 ReplArgs = pure ()
main0 (SessionClear sid) = pure ()
main0 (SessionShow sid) = pure ()
main0 (SessionPrompt sid p) = pure ()
main0 (TestRecorderArgs provider stream) = do
  print provider
  -- print model
  print stream

main1 :: IO ()
main1 = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  AllModels {gemini_2_5_flash, claude_haiku_4_5, llama_3_2, gpt_4_1, gpt_5_nano} <- getAllModels
  testExample False llama_3_2

runtimeArgsParser :: Parser RuntimeArgs
runtimeArgsParser = testRecorderArgs <|> replArgs <|> sessionClear <|> sessionPrompt <|> sessionShow

data RuntimeArgs
  = TestRecorderArgs
      { provider :: String,
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

testRecorderArgs :: Parser RuntimeArgs
testRecorderArgs =
  TestRecorderArgs
    <$> strOption
      ( long "test"
          <> metavar "PROVIDER"
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
