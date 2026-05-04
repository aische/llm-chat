module Adapters.AgentTask (agentTask) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM
  ( ChatEnv (..),
    ChatStep (..),
    Conversation (..),
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
    buildChatStep,
    emptyUsage,
    fileStore,
    resumeSession,
    runStepServer,
    safeHooks,
  )
import LLM.Core.Logger (Hooks (..), LogLevel (..))
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
      mc = envModel env
      hooks = safeHooks (envHooks env)

  -- Try to resume an existing session first
  mStep <- resumeSession store sid env mc
  step <- case (mStep, mPrompt) of
    (Just s, _) -> do
      putStrLn $ "[Resuming session " <> T.unpack sid <> " from checkpoint]"
      pure s
    (Nothing, Just prompt) -> do
      -- Check if session already completed
      mState <- loadSession store sid
      case mState of
        Just (SessionState _ _ _ (Completed answer)) -> do
          putStrLn "[Session already completed]"
          TIO.putStrLn answer
          fail "Session already completed"
        Just (SessionState _ _ _ (Failed err)) -> do
          putStrLn $ "[Session previously failed: " <> show err <> "]"
          putStrLn "[Starting fresh with new prompt]"
          newSession env prompt
        _ -> newSession env prompt
    (Nothing, Nothing) -> do
      putStrLn "No session to resume and no prompt given."
      fail "Nothing to do"

  -- Run with the server interpreter (checkpoints at every tool round)
  let call = providerChat (mcGateway mc) hooks
  putStrLn $ "[Running agent task: " <> T.unpack sid <> "]"
  result <-
    runStepServer
      store
      sid
      hooks
      (envAbortSignal env)
      (envTools env)
      (envContextWindow env)
      (mcRetry mc)
      (mcRequestTimeout mc)
      call
      step

  -- Print result
  case result of
    Left (err, _conv, usage) -> do
      putStrLn $ "\n[Agent failed: " <> show err <> "]"
      printUsage usage
    Right (answer, conv, usage) -> do
      putStrLn "\n[Agent completed]"
      TIO.putStrLn answer
      printUsage usage
      printToolSummary conv

newSession :: ChatEnv -> Text -> IO ChatStep
newSession env prompt = do
  let mc = envModel env
      conv = Conversation [UserTurn prompt]
  pure $ buildChatStep env mc 0 emptyUsage conv

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
