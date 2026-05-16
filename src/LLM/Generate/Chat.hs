module LLM.Generate.Chat
  ( generateTextSimple,
    streamTextSimple,
    simpleChatStepInterpreter,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import LLM.Core.Abort (isAborted)
import LLM.Core.Logger (Hooks (..))
import LLM.Core.Types
  ( Conversation (..),
    LLMError (..),
    StreamEvent,
    ToolContext (..),
  )
import LLM.Core.Usage (Usage)
import LLM.Core.Utils (executeToolsWithAbort, withRetry, withTimeout)
import LLM.Generate.ChatStep (ChatStep (..), windowOffset)
import LLM.Generate.ChatStepInterpreter (ChatStepInterpreter, generateTextWith, streamTextWith)
import LLM.Generate.Types
  ( ChatEnv (..),
    WorkerMap,
  )

-- | Run a non-streaming chat. Uses the standard in-memory interpreter.
generateTextSimple ::
  (MonadIO m) =>
  Maybe WorkerMap ->
  ChatEnv ->
  Conversation ->
  Text ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
generateTextSimple mbWorkerMap = generateTextWith simpleChatStepInterpreter mbGenWorkerMap
  where
    mbGenWorkerMap = fmap (generateTextSimple mbWorkerMap,) mbWorkerMap

-- | Like 'generateTextSimple', but streams text deltas via a callback.
streamTextSimple ::
  (MonadIO m) =>
  Maybe WorkerMap ->
  ChatEnv ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamTextSimple mbWorkerMap unsafeEnv conv msg callback = streamTextWith simpleChatStepInterpreter mbGenWorkerMap unsafeEnv conv msg callback
  where
    mbGenWorkerMap = fmap (\c d t -> streamTextSimple mbWorkerMap c d t callback,) mbWorkerMap

-- | Standard IO interpreter for 'ChatStep'. Executes effects directly:
-- logging, throttling, LLM calls (with retry/timeout), and tool execution.
simpleChatStepInterpreter :: (MonadIO m) => ChatStepInterpreter m
simpleChatStepInterpreter hooks abortSig tools ctxWindow retryPolicy reqTimeout call = go
  where
    go :: (MonadIO m) => ChatStep -> m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
    go (Done result) = pure result
    go (Log _level msg next) = do
      liftIO $ onLog hooks _level msg
      go next
    go (Throttle ms next) = do
      liftIO $ threadDelay (ms * 1000)
      go next
    go (CheckAbort k) = do
      aborted <- liftIO $ maybe (pure False) isAborted abortSig
      go (k aborted)
    go (CallLLM req k) = do
      result <-
        liftIO $
          withTimeout reqTimeout $
            withRetry retryPolicy (onLog hooks) $
              call req
      go (k result)
    go step@ExecTools {} = do
      let conv = esConv step
          usage = esUsage step
          offset = windowOffset ctxWindow conv
          ctx =
            ToolContext
              { tcConversation = conv,
                tcUsage = usage,
                tcWindowOffset = offset,
                tcAbortSignal = abortSig
              }
      results <- liftIO $ executeToolsWithAbort abortSig ctx tools (esCalls step)
      go (esCont step results)
