{-# LANGUAGE LambdaCase #-}

module Adapters.AgentTask (agentTask) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM
  ( ChatEnv (..),
    Conversation (..),
    Hooks (..),
    LLMProvider (..),
    ModelConfig (..),
    SessionId,
    SessionState (..),
    SessionStatus (..),
    Store (..),
    StreamEvent (..),
    ToolResult (trName),
    Turn (..),
    Usage (usageInputTokens, usageOutputTokens, usageTotalCost),
    fileStore,
    resumeSession,
    safeHooks,
    sessionChatStepInterpreter,
    streamChatWith,
  )
import System.Environment (getArgs, getProgName)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Printf (printf)

-- | Autonomous agent adapter using 'runStepServer' with file-based
-- checkpointing. The agent runs a multi-turn tool loop to completion,
-- persisting state after every tool round. If killed, re-running with
-- the same session ID resumes from the last checkpoint.
--
-- Usage:
--   cabal run hello-haskell1 -- <session-id> "Refactor all files to use X"
--   cabal run hello-haskell1 -- <session-id>           # resume interrupted session
--   cabal run hello-haskell1 -- <session-id> --status  # check session status
agentTask :: ChatEnv -> IO ()
agentTask env = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    [sid, "--status"] -> showStatus sid
    [sid, prompt] -> runOrResume env (T.pack sid) (Just (T.pack prompt))
    [sid] -> runOrResume env (T.pack sid) Nothing
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " <> progName <> " <session-id> \"<goal>\""
      putStrLn $ "       " <> progName <> " <session-id>              (resume)"
      putStrLn $ "       " <> progName <> " <session-id> --status"

sessionsDir :: FilePath
sessionsDir = ".sessions"

showStatus :: String -> IO ()
showStatus sid = do
  let store = fileStore sessionsDir
  mState <- loadSession store (T.pack sid)
  case mState of
    Nothing -> putStrLn $ "No session found: " <> sid
    Just (SessionState conv usage rounds status) -> do
      putStrLn $ "Session: " <> sid
      putStrLn $ "Status:  " <> showStatus' status
      printf "Rounds:  %d\n" rounds
      printf "Turns:   %d\n" (length $ unConversation conv)
      printf "Tokens:  %d in + %d out\n" (usageInputTokens usage) (usageOutputTokens usage)
      printf "Cost:    $%.4f\n" (usageTotalCost usage)
  where
    showStatus' Pending = "pending"
    showStatus' Running = "running (may be interrupted)"
    showStatus' Suspended = "suspended (safe to resume)"
    showStatus' (Completed _) = "completed"
    showStatus' (Failed err) = "failed: " <> show err

runOrResume :: ChatEnv -> SessionId -> Maybe Text -> IO ()
runOrResume env sid mPrompt = do
  let store = fileStore sessionsDir
      interp = sessionChatStepInterpreter store sid -- partially applied = ChatStepInterpreter
  mState <- loadSession store sid
  case (mState, mPrompt) of
    -- Already completed
    (Just (SessionState _ _ _ (Completed answer)), _) -> do
      putStrLn "[Session already completed]"
      TIO.putStrLn answer

    -- Previously failed, start fresh with new prompt
    (Just (SessionState _ _ _ (Failed err)), Just prompt) -> do
      putStrLn $ "[Session previously failed: " <> show err <> ", starting fresh]"
      runNew interp prompt

    -- Suspended or Running (interrupted) — resume
    (Just (SessionState conv usage _ _), _) -> do
      putStrLn $ "[Resuming session " <> T.unpack sid <> " from checkpoint]"
      -- Resume: the conversation already has the user message from the
      -- original run. We need to re-enter the tool loop mid-turn.
      -- streamChatWith would prepend a new UserTurn, so we use the
      -- interpreter directly with a reconstructed ChatStep.
      let mc = envModel env
      mStep <- resumeSession store sid env mc
      case mStep of
        Just step -> do
          putStrLn $ "[Running agent task: " <> T.unpack sid <> "]"
          let hooks = safeHooks (envHooks env)
              call req = providerChatStream (mcGateway mc) hooks req $ \case
                StreamDelta txt -> TIO.putStr txt
                StreamToolCall tc -> TIO.putStrLn $ "  [tool call: " <> T.pack (show tc) <> "]"
          result <-
            interp
              hooks
              (envAbortSignal env)
              (envTools env)
              (envContextWindow env)
              (mcRetry mc)
              (mcRequestTimeout mc)
              call
              step
          printResult result
        Nothing -> putStrLn "Nothing to resume."

    -- No session, need a prompt
    (Nothing, Just prompt) -> runNew interp prompt
    (Nothing, Nothing) -> putStrLn "No session to resume and no prompt given."
  where
    runNew interp prompt = do
      putStrLn $ "[Running agent task: " <> T.unpack sid <> "]"
      result <- streamChatWith interp env (Conversation []) prompt $ \case
        StreamDelta txt -> TIO.putStr txt
        StreamToolCall tc -> TIO.putStrLn $ "  [tool call: " <> T.pack (show tc) <> "]"
      printResult result

    printResult result = case result of
      Left (err, _, usage) -> do
        putStrLn $ "\n[Agent failed: " <> show err <> "]"
        printUsage usage
      Right (_, conv, usage) -> do
        putStrLn "\n[Agent completed]"
        printUsage usage
        printToolSummary conv

printUsage :: Usage -> IO ()
printUsage usage =
  printf
    "  (%d in + %d out tokens, $%.4f)\n"
    (usageInputTokens usage)
    (usageOutputTokens usage)
    (usageTotalCost usage)

printToolSummary :: Conversation -> IO ()
printToolSummary (Conversation turns) = do
  let toolCalls = [r | ToolTurn results <- turns, r <- results]
  if null toolCalls
    then pure ()
    else do
      printf "  Tools used: %d calls\n" (length toolCalls)
      mapM_ (\r -> TIO.putStrLn $ "    - " <> trName r) toolCalls
