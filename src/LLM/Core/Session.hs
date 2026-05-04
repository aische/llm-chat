{-# LANGUAGE DeriveGeneric #-}

module LLM.Core.Session
  ( SessionId,
    SessionState (..),
    SessionStatus (..),
    Store (..),
    fileStore,
    sessionChatStepInterpreter,
    resumeSession,
  )
where

import Control.Concurrent (threadDelay)
import Control.Retry (RetryPolicyM, RetryStatus (..), retrying, rsIterNumber)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Abort (AbortSignal, isAborted)
import LLM.Core.ChatStep
  ( ChatStep (..),
    buildChatStep,
    windowOffset,
  )
import LLM.Core.ChatStepInterpreter (ChatStepInterpreter)
import LLM.Core.LLMProvider (ChatEnv (..), ModelConfig (..))
import LLM.Core.Logger (Hooks (..), LogLevel (..), Logger)
import LLM.Core.Types
  ( ChatRequest,
    Conversation (..),
    LLMError (TimeoutError),
    LLMResult,
    Tool,
    ToolContext (..),
    Turn (AssistantTurn, ToolTurn),
  )
import LLM.Core.Usage (Usage, emptyUsage)
import LLM.Core.Utils (executeToolsWithAbort, isRetryable, withRetry, withTimeout)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Timeout (timeout)

type SessionId = Text

-- | Serializable snapshot of a chat session.
data SessionState = SessionState
  { ssConversation :: Conversation,
    ssUsage :: Usage,
    ssRounds :: Int,
    ssStatus :: SessionStatus
  }
  deriving (Show, Eq, Generic)

instance ToJSON SessionState

instance FromJSON SessionState

data SessionStatus
  = -- | Not started yet
    Pending
  | -- | Actively executing (LLM call or tools in flight)
    Running
  | -- | Paused between rounds, safe to resume
    Suspended
  | -- | Finished with a final answer
    Completed Text
  | -- | Terminated with an error
    Failed LLMError
  deriving (Show, Eq, Generic)

instance ToJSON SessionStatus

instance FromJSON SessionStatus

-- | Abstract persistence backend.
data Store = Store
  { loadSession :: SessionId -> IO (Maybe SessionState),
    saveSession :: SessionId -> SessionState -> IO ()
  }

-- | File-based store. Each session is a JSON file in the given directory.
fileStore :: FilePath -> Store
fileStore dir =
  Store
    { loadSession = \sid -> do
        let path = dir </> T.unpack sid <> ".json"
        exists <- doesFileExist path
        if exists
          then do
            result <- eitherDecodeFileStrict' path
            case result of
              Right st -> pure (Just st)
              Left _ -> pure Nothing
          else pure Nothing,
      saveSession = \sid st -> do
        createDirectoryIfMissing True dir
        let path = dir </> T.unpack sid <> ".json"
        encodeFile path st
    }

-- | Reconstruct a 'ChatStep' from a saved session state.
-- Returns 'Nothing' if the session is already terminal (completed/failed)
-- or has never been saved.
resumeSession :: Store -> SessionId -> ChatEnv -> ModelConfig -> IO (Maybe ChatStep)
resumeSession store sid env mc = do
  mState <- loadSession store sid
  pure $ case mState of
    Nothing -> Nothing
    Just (SessionState _ _ _ (Completed _)) -> Nothing
    Just (SessionState _ _ _ (Failed _)) -> Nothing
    Just (SessionState conv usage rounds _) ->
      Just (buildChatStep env mc rounds usage conv)

-- | Server interpreter for 'ChatStep'. Like 'runStepIO' but persists
-- session state at checkpoint boundaries (before/after tool execution
-- and at terminal states). If the process crashes, 'resumeSession'
-- reconstructs the program from the last checkpoint.
sessionChatStepInterpreter :: Store -> SessionId -> ChatStepInterpreter
sessionChatStepInterpreter store sid hooks abortSig tools ctxWindow retryPolicy reqTimeout call = go
  where
    go (Done result) = do
      -- Persist terminal state
      case result of
        Left (err, conv, usage) ->
          saveSession store sid (SessionState conv usage 0 (Failed err))
        Right (txt, conv, usage) ->
          saveSession store sid (SessionState conv usage 0 (Completed txt))
      pure result
    go (Log level msg next) = do
      onLog hooks level msg
      go next
    go (Throttle ms next) = do
      threadDelay (ms * 1000)
      go next
    go (CheckAbort k) = do
      aborted <- maybe (pure False) isAborted abortSig
      go (k aborted)
    go (CallLLM req k) = do
      result <-
        withTimeout reqTimeout $
          withRetry retryPolicy (onLog hooks) $
            call req
      go (k result)
    go step@ExecTools {} = do
      let conv = esConv step
          usage = esUsage step
          round_ = esRound step
          respTxt = esRespText step
          calls = esCalls step

      -- Checkpoint: about to execute tools
      saveSession store sid (SessionState conv usage round_ Running)

      let offset = windowOffset (Just (fromMaybe maxBound ctxWindow)) conv
          ctx =
            ToolContext
              { tcConversation = conv,
                tcUsage = usage,
                tcWindowOffset = offset,
                tcAbortSignal = abortSig
              }
      results <- executeToolsWithAbort abortSig ctx tools calls

      -- Checkpoint: tools completed, save updated conversation
      case results of
        Right toolResults -> do
          let conv' =
                Conversation
                  ( unConversation conv
                      ++ [AssistantTurn respTxt calls]
                      ++ [ToolTurn toolResults]
                  )
          saveSession store sid (SessionState conv' usage (round_ + 1) Suspended)
        Left _ ->
          pure () -- aborted, terminal state handled by Done
      go (esCont step results)
