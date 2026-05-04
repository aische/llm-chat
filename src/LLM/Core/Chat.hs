module LLM.Core.Chat (runChat, streamChat, runStepIO) where

import Control.Concurrent (threadDelay)
import Control.Retry (RetryPolicyM, RetryStatus (..), retrying, rsIterNumber)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Abort (AbortSignal, isAborted)
import LLM.Core.ChatStep (ChatStep (..), buildChatStep, windowOffset)
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
import LLM.Core.Utils (executeToolsWithAbort, isRetryable)
import System.Timeout (timeout)

-- | Run a non-streaming chat with automatic tool-call handling.
-- Tries each model in 'envModels' in order, falling back on retryable errors.
runChat ::
  ChatEnv ->
  Conversation ->
  Text ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
runChat unsafeEnv conv msg = do
  let conv' = Conversation (unConversation conv ++ [UserTurn msg])
      env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  onLog (envHooks env) Info $ "runChat: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv' $ \mc c u ->
    let call = providerChat (mcGateway mc) (envHooks env)
        step = buildChatStep env mc 0 u c
     in runStepIO (envHooks env) (envAbortSignal env) (envTools env) (envContextWindow env) (mcRetry mc) (mcRequestTimeout mc) call step

-- | Like 'runChat', but streams text deltas via a callback as they arrive.
streamChat ::
  ChatEnv ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamChat unsafeEnv conv msg callback = do
  let conv' = Conversation (unConversation conv ++ [UserTurn msg])
      env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  onLog (envHooks env) Info $ "streamChat: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv' $ \mc c u ->
    let call req = providerChatStream (mcGateway mc) (envHooks env) req callback
        step = buildChatStep env mc 0 u c
     in runStepIO (envHooks env) (envAbortSignal env) (envTools env) (envContextWindow env) (mcRetry mc) (mcRequestTimeout mc) call step

-- | Try each 'ModelConfig' in order. Falls back on retryable errors.
-- On fallback, the next model continues from the partial conversation
-- and accumulated usage of the failed model, rather than starting over.
withFallback ::
  ChatEnv ->
  Conversation ->
  (ModelConfig -> Conversation -> Usage -> IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))) ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
withFallback env conv tryModel = go (envModel env : envFallbacks env) conv emptyUsage
  where
    go [] c u = pure $ Left (NetworkError "all models failed", c, u)
    go [mc] c u = do
      onLog (envHooks env) Info $ "Using model: " <> mcModel mc <> " via " <> providerName (mcGateway mc)
      result <- tryModel mc c u
      pure $ case result of
        Left (err, c', u') -> Left (err, c', u')
        Right r -> Right r
    go (mc : rest) c u = do
      onLog (envHooks env) Info $ "Trying model: " <> mcModel mc <> " via " <> providerName (mcGateway mc)
      result <- tryModel mc c u
      case result of
        Left (Aborted, c', u') -> pure $ Left (Aborted, c', u')
        Left (err, c', u') -> do
          onLog (envHooks env) Warn $ "Falling back from " <> mcModel mc <> ": " <> T.pack (show err)
          go rest c' u'
        Right r -> pure $ Right r

-- | Standard IO interpreter for 'ChatStep'. Executes effects directly:
-- logging, throttling, LLM calls (with retry/timeout), and tool execution.
runStepIO ::
  Hooks ->
  Maybe AbortSignal ->
  [LLM.Core.Types.Tool] ->
  Maybe Int ->
  RetryPolicyM IO ->
  Maybe Int ->
  (ChatRequest -> IO LLMResult) ->
  ChatStep ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
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

-- | Wrap an action with a timeout (ms). Returns 'TimeoutError' on expiry.
withTimeout :: Maybe Int -> IO LLMResult -> IO LLMResult
withTimeout Nothing action = action
withTimeout (Just ms) action = do
  result <- timeout (ms * 1000) action
  pure $ fromMaybe (Left TimeoutError) result

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
