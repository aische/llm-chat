module Main where

import Adapters.Repl (replMain)
import Adapters.SessionChat (SessionCommand (ClearSession, PromptSession, ShowSession), sessionChatMain)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.Text qualified as T
import Example1 qualified as E1
import Example2 qualified as E2
import Example3 qualified as E3
import Example4 qualified as E4
import Example5 qualified as E5
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

mainInternal :: RuntimeArgs -> IO ()
mainInternal args = do
  case args of
    ReplArgs -> replMain
    (TestRecorderArgs name stream) -> do
      print name
      print stream
      testExample name stream
    SessionClear -> sessionChatMain ClearSession
    SessionShow -> sessionChatMain ShowSession
    SessionPrompt p -> sessionChatMain (PromptSession (T.pack p))
    Example1 -> E1.main
    Example2 -> E2.main
    Example3 -> E3.main
    Example4 -> E4.main
    Example5 -> E5.main

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
  | Example4
  | Example5

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
        <> command "example4" (info (pure Example4) (progDesc "Example4 using json configs for models and envs"))
        <> command "example5" (info (pure Example5) (progDesc "Example5"))
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
