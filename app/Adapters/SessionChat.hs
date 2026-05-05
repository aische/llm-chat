{-# LANGUAGE LambdaCase #-}

module Adapters.SessionChat (sessionChat) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM
  ( ChatEnv,
    Conversation (..),
    StreamEvent (..),
    ToolResult (trName),
    Turn (..),
    Usage (usageInputTokens, usageOutputTokens, usageTotalCost),
    addUsage,
    emptyUsage,
    streamChat,
  )
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Printf (printf)

-- | Persistent conversation state saved between invocations.
data SessionFile = SessionFile
  { sfConversation :: Conversation,
    sfUsage :: Usage
  }
  deriving (Show, Generic)

instance ToJSON SessionFile

instance FromJSON SessionFile

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

-- | CLI adapter: takes the prompt from command-line args, prints the
-- streamed response, persists the conversation for next invocation.
--
-- Usage:
--   cabal run hello-haskell1 -- "What is the weather?"
--   cabal run hello-haskell1 -- "And in Berlin?"    # continues conversation
--   cabal run hello-haskell1 -- --clear              # reset session
--   cabal run hello-haskell1 -- --history            # show conversation so far
sessionChat :: ChatEnv -> IO ()
sessionChat env = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["--clear"] -> do
      saveSessionFile freshSession
      putStrLn "Session cleared."
    ["--history"] -> do
      sf <- loadSessionFile
      printHistory (sfConversation sf) (sfUsage sf)
    [] -> do
      progName <- getProgName
      putStrLn $ "Usage: " <> progName <> " \"<prompt>\""
      putStrLn $ "       " <> progName <> " --history"
      putStrLn $ "       " <> progName <> " --clear"
    promptParts -> do
      let prompt = T.pack (unwords promptParts)
      sf <- loadSessionFile
      result <- streamChat env (sfConversation sf) prompt $ \case
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
