module LLM.Core.Chat
  ( generateTextSimple,
    streamTextSimple,
    simpleChatStepInterpreter,
  )
where

import Control.Concurrent (threadDelay)
-- import Control.Retry (RetryPolicyM, RetryStatus (..), retrying, rsIterNumber)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
-- import Data.Text qualified as T
import LLM.Core.Abort (isAborted)
import LLM.Core.ChatStep (ChatStep (..), windowOffset)
import LLM.Core.ChatStepInterpreter (ChatStepInterpreter, generateTextWith, streamTextWith)
import LLM.Core.Logger (Hooks (..))
import LLM.Core.Types
  ( ChatEnv (..),
    Conversation (..),
    LLMError (..),
    StreamEvent,
    ToolContext (..),
  )
import LLM.Core.Usage (Usage)
import LLM.Core.Utils (executeToolsWithAbort, withRetry, withTimeout)

-- import System.Timeout (timeout)

-- | Run a non-streaming chat. Uses the standard in-memory interpreter.
generateTextSimple ::
  ChatEnv ->
  Conversation ->
  Text ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
generateTextSimple = generateTextWith simpleChatStepInterpreter

-- | Like 'generateTextSimple', but streams text deltas via a callback.
streamTextSimple ::
  ChatEnv ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamTextSimple = streamTextWith simpleChatStepInterpreter

-- | Standard IO interpreter for 'ChatStep'. Executes effects directly:
-- logging, throttling, LLM calls (with retry/timeout), and tool execution.
simpleChatStepInterpreter :: ChatStepInterpreter
simpleChatStepInterpreter hooks abortSig tools ctxWindow retryPolicy reqTimeout call = go
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
