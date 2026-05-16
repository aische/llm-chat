module Adapters.SessionChat (sessionChat, sessionChatMain, SessionCommand (..)) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM (LogLevel (Debug), noHooks, withJsonDump, withStderrLogger)
import LLM.Core.Types
  ( Conversation (..),
    StreamEvent (..),
    ToolResult (trName),
    Turn (..),
  )
import LLM.Core.Usage
  ( Usage (usageInputTokens, usageOutputTokens, usageTotalCost),
    addUsage,
    emptyUsage,
  )
import LLM.Generate.Generate (streamText)
import LLM.Generate.Types
  ( ChatEnv (..),
  )
import LLM.Load.LoadEnvs (defaultEnvFilePaths, loadDefaultEnvOrThrow)
import System.Directory (doesFileExist)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Printf (printf)

-- | Persistent conversation state saved between invocations.
data SessionFile = SessionFile
  { sfConversation :: Conversation,
    sfUsage :: Usage
  }
  deriving (Show, Generic, ToJSON, FromJSON)

sessionPath :: FilePath
sessionPath = ".session.json"

-- | Load saved session or start fresh.
loadSessionFile :: IO SessionFile
loadSessionFile = do
  exists <- doesFileExist sessionPath
  if exists
    then do
      result <- eitherDecodeFileStrict' sessionPath
      case result of
        Right sf -> pure sf
        Left err -> do
          putStrLn $ "Warning: could not load session (" <> err <> "), starting fresh."
          pure freshSession
    else pure freshSession

freshSession :: SessionFile
freshSession = SessionFile (Conversation []) emptyUsage

saveSessionFile :: SessionFile -> IO ()
saveSessionFile = encodeFile sessionPath

data SessionCommand
  = ClearSession
  | ShowSession
  | PromptSession Text

sessionChatMain :: SessionCommand -> IO ()
sessionChatMain command = do
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
  env <- loadDefaultEnvOrThrow defaultEnvFilePaths hooks
  sessionChat env command

sessionChat :: ChatEnv -> SessionCommand -> IO ()
sessionChat env command = do
  hSetBuffering stdout NoBuffering
  case command of
    ClearSession -> do
      saveSessionFile freshSession
      putStrLn "Session cleared."
    ShowSession -> do
      sf <- loadSessionFile
      printHistory (sfConversation sf) (sfUsage sf)
    PromptSession prompt -> do
      sf <- loadSessionFile
      result <- streamText env (sfConversation sf) prompt $ \case
        StreamDelta txt -> TIO.putStr txt
        StreamToolCall tc -> TIO.putStrLn $ "  [tool call: " <> T.pack (show tc) <> "]"
      case result of
        Left (err, _, _) -> do
          putStrLn $ "\nError: " <> show err
        Right (_, conv', usage) -> do
          putStrLn ""
          let totalUsage = addUsage (sfUsage sf) usage
          printf
            "  (%d turns, %d in + %d out tokens, session total: $%.4f)\n"
            (length $ unConversation conv')
            (usageInputTokens usage)
            (usageOutputTokens usage)
            (usageTotalCost totalUsage)
          saveSessionFile (SessionFile conv' totalUsage)

printHistory :: Conversation -> Usage -> IO ()
printHistory (Conversation []) _ = putStrLn "(empty session)"
printHistory (Conversation turns) usage = do
  mapM_ printTurn turns
  printf
    "\nSession total: %d turns, $%.4f\n"
    (length turns)
    (usageTotalCost usage)
  where
    printTurn (UserTurn msg) =
      TIO.putStrLn $ "> " <> msg
    printTurn (AssistantTurn msg _) =
      TIO.putStrLn msg
    printTurn (ToolTurn results) =
      mapM_ (\r -> TIO.putStrLn $ "  [tool: " <> trName r <> "]") results
