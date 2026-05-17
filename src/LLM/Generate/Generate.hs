module LLM.Generate.Generate
  ( generateText,
    generateTextConversation,
    generateTextWithWorkers,
    generateTextConversationWithWorkers,
    streamText,
    streamTextWithWorkers,
    streamTextConversation,
    streamTextConversationWithWorkers,
    simpleChatStepInterpreter,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
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
import LLM.Generate.ChatStepInterpreter
  ( ChatStepInterpreter,
    generateTextConversationWith,
    generateTextWith,
    streamTextConversationWith,
    streamTextWith,
  )
import LLM.Generate.Types
  ( ChatEnv (..),
    GenerateText,
    GeneratedResult,
    WorkerMap,
  )

generateText ::
  (MonadUnliftIO m, MonadCatch m) =>
  ChatEnv m ->
  Conversation ->
  Text ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
generateText = generateTextWithWorkers Nothing

generateTextConversation ::
  (MonadUnliftIO m, MonadCatch m) =>
  Maybe (GenerateText m, WorkerMap m) ->
  ChatEnv m ->
  Conversation ->
  m (GeneratedResult (Text, Conversation, Usage))
generateTextConversation = generateTextConversationWith simpleChatStepInterpreter

-- | Run a non-streaming chat. Uses the standard in-memory interpreter.
generateTextWithWorkers ::
  (MonadUnliftIO m, MonadCatch m) =>
  Maybe (WorkerMap m) ->
  ChatEnv m ->
  Conversation ->
  Text ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
generateTextWithWorkers mbWorkerMap = generateTextWith simpleChatStepInterpreter mbGenWorkerMap
  where
    mbGenWorkerMap = fmap (generateTextWithWorkers mbWorkerMap,) mbWorkerMap

generateTextConversationWithWorkers ::
  (MonadUnliftIO m, MonadCatch m) =>
  Maybe (WorkerMap m) ->
  ChatEnv m ->
  Conversation ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
generateTextConversationWithWorkers mbWorkerMap = generateTextConversationWith simpleChatStepInterpreter mbGenWorkerMap
  where
    mbGenWorkerMap = fmap (generateTextWithWorkers mbWorkerMap,) mbWorkerMap

-- | Like 'generateTextSimple', but streams text deltas via a callback.
streamText ::
  (MonadUnliftIO m, MonadCatch m) =>
  ChatEnv m ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamText = streamTextWithWorkers Nothing

streamTextWithWorkers ::
  (MonadUnliftIO m, MonadCatch m) =>
  Maybe (WorkerMap m) ->
  ChatEnv m ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamTextWithWorkers mbWorkerMap unsafeEnv conv msg callback = streamTextWith simpleChatStepInterpreter mbGenWorkerMap unsafeEnv conv msg callback
  where
    mbGenWorkerMap = fmap (\c d t -> streamTextWithWorkers mbWorkerMap c d t callback,) mbWorkerMap

streamTextConversation ::
  (MonadUnliftIO m, MonadCatch m) =>
  ChatEnv m ->
  Conversation ->
  (StreamEvent -> IO ()) ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamTextConversation = streamTextConversationWithWorkers Nothing

streamTextConversationWithWorkers ::
  (MonadUnliftIO m, MonadCatch m) =>
  Maybe (WorkerMap m) ->
  ChatEnv m ->
  Conversation ->
  (StreamEvent -> IO ()) ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamTextConversationWithWorkers mbWorkerMap unsafeEnv conv callback = streamTextConversationWith simpleChatStepInterpreter mbGenWorkerMap unsafeEnv conv callback
  where
    mbGenWorkerMap = fmap (\c d t -> streamTextWithWorkers mbWorkerMap c d t callback,) mbWorkerMap

-- | Standard IO interpreter for 'ChatStep'. Executes effects directly:
-- logging, throttling, LLM calls (with retry/timeout), and tool execution.
simpleChatStepInterpreter ::
  (MonadUnliftIO m, MonadCatch m) =>
  ChatStepInterpreter m
simpleChatStepInterpreter hooks abortSig tools ctxWindow retryPolicy reqTimeout call = go
  where
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
      results <- executeToolsWithAbort abortSig ctx tools (esCalls step)
      go (esCont step results)
