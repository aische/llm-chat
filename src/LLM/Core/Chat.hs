module LLM.Core.Chat
  ( runChatSimple,
    streamChatSimple,
    runStepIO,
  )
where

import Control.Concurrent (threadDelay)
import Control.Retry (RetryPolicyM, RetryStatus (..), retrying, rsIterNumber)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Abort (AbortSignal, isAborted)
import LLM.Core.ChatStep (ChatStep (..), buildChatStep, windowOffset)
import LLM.Core.ChatStepInterpreter (ChatStepInterpreter, runChatWith, streamChatWith, withFallback)
import LLM.Core.LLMProvider (ChatEnv (..), LLMProvider (..), ModelConfig (..))
import LLM.Core.Logger (Hooks (..), LogLevel (..), Logger, safeHooks)
import LLM.Core.Types
  ( ChatRequest,
    Conversation (..),
    LLMError (Aborted, NetworkError, TimeoutError),
    LLMResult,
    StreamEvent,
    Tool,
    ToolContext (..),
    Turn (UserTurn),
  )
import LLM.Core.Usage (Usage, emptyUsage)
import LLM.Core.Utils (executeToolsWithAbort, isRetryable, withTimeout)
import System.Timeout (timeout)

-- | Run a non-streaming chat. Uses the standard in-memory interpreter.
runChatSimple ::
  ChatEnv ->
  Conversation ->
  Text ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
runChatSimple = runChatWith runStepIO

-- | Like 'runChatSimple', but streams text deltas via a callback.
streamChatSimple ::
  ChatEnv ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamChatSimple = streamChatWith runStepIO

-- | Standard IO interpreter for 'ChatStep'. Executes effects directly:
-- logging, throttling, LLM calls (with retry/timeout), and tool execution.
runStepIO :: ChatStepInterpreter
runStepIO hooks abortSig tools ctxWindow retryPolicy reqTimeout call = go
  where
    go (Done result) = pure result
    go (Log _level msg next) = do
      onLog hooks _level msg
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
          offset = windowOffset (Just (fromMaybe maxBound ctxWindow)) conv
          ctx =
            ToolContext
              { tcConversation = conv,
                tcUsage = usage,
                tcWindowOffset = offset,
                tcAbortSignal = abortSig
              }
      results <- executeToolsWithAbort abortSig ctx tools (esCalls step)
      go (esCont step results)

-- | Retry an action using the retry package's policy (exponential backoff + jitter).
withRetry :: RetryPolicyM IO -> Logger -> IO LLMResult -> IO LLMResult
withRetry policy log action =
  retrying
    policy
    ( \status result -> case result of
        Left err | isRetryable err -> do
          log Warn $
            "Retryable error (attempt "
              <> T.pack (show (rsIterNumber status + 1))
              <> "): "
              <> T.pack (show err)
          pure True
        _ -> pure False
    )
    (const action)
