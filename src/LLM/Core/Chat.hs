module LLM.Core.Chat
  ( runChatWith,
    streamChatWith,
    StepInterpreter,
    withFallback,
  )
where

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
import LLM.Core.Utils (executeToolsWithAbort, isRetryable, withTimeout)
import System.Timeout (timeout)

-- | A step interpreter runs a 'ChatStep' program to completion.
-- Both 'runStepIO' and @runStepServer store sid@ satisfy this type.
type StepInterpreter =
  Hooks ->
  Maybe AbortSignal ->
  [Tool] ->
  Maybe Int -> -- context window
  RetryPolicyM IO ->
  Maybe Int -> -- request timeout
  (ChatRequest -> IO LLMResult) ->
  ChatStep ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))

-- | Generic non-streaming chat with a pluggable interpreter.
-- Use with @runStepServer store sid@ for persistence, or any custom interpreter.
runChatWith ::
  StepInterpreter ->
  ChatEnv ->
  Conversation ->
  Text ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
runChatWith interp unsafeEnv conv msg = do
  let conv' = Conversation {unConversation = unConversation conv ++ [UserTurn msg]}
      env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  onLog (envHooks env) Info $ "runChat: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv' $ \mc c u ->
    let call = providerChat (mcGateway mc) (envHooks env)
        step = buildChatStep env mc 0 u c
     in interp (envHooks env) (envAbortSignal env) (envTools env) (envContextWindow env) (mcRetry mc) (mcRequestTimeout mc) call step

-- | Generic streaming chat with a pluggable interpreter.
streamChatWith ::
  StepInterpreter ->
  ChatEnv ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamChatWith interp unsafeEnv conv msg callback = do
  let conv' = Conversation {unConversation = unConversation conv ++ [UserTurn msg]}
      env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  onLog (envHooks env) Info $ "streamChat: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv' $ \mc c u ->
    let call req = providerChatStream (mcGateway mc) (envHooks env) req callback
        step = buildChatStep env mc 0 u c
     in interp (envHooks env) (envAbortSignal env) (envTools env) (envContextWindow env) (mcRetry mc) (mcRequestTimeout mc) call step

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
