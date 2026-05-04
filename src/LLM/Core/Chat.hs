module LLM.Core.Chat (runChat, streamChat) where

import Control.Concurrent (threadDelay)
import Control.Retry (RetryPolicyM, RetryStatus (..), retrying, rsIterNumber)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Abort (AbortSignal, isAborted)
import LLM.Core.LLMProvider (ChatEnv (..), LLMProvider (..), ModelConfig (..))
import LLM.Core.Logger (Hooks (..), LogLevel (..), Logger, safeHooks)
import LLM.Core.Types
  ( ChatRequest (..),
    ChatResponse (respText, respUsage),
    Conversation,
    LLMError (Aborted, NetworkError, TimeoutError, ToolLoopExceeded),
    LLMResult,
    StreamEvent,
    Tool (toolDef),
    ToolCall (tcName),
    ToolContext (..),
    ToolResult (trContent, trName),
    Turn (AssistantTurn, ToolTurn, UserTurn),
  )
import LLM.Core.Usage (Usage (..), addUsage, emptyUsage, estimateCost)
import LLM.Core.Utils
  ( executeToolsWithAbort,
    getToolCalls,
    hasToolCalls,
    isRetryable,
  )
import System.Timeout (timeout)

-- | Run a non-streaming chat with automatic tool-call handling.
-- Tries each model in 'envModels' in order, falling back on retryable errors.
runChat ::
  ChatEnv ->
  Conversation ->
  Text ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
runChat unsafeEnv conv msg = do
  let conv' = conv ++ [UserTurn msg]
      env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  onLog (envHooks env) Info $ "runChat: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv' $ \mc c u ->
    let call = providerChat (mcGateway mc) (envHooks env)
     in chatLoop env mc call 0 u c

-- | Like 'runChat', but streams text deltas via a callback as they arrive.
streamChat ::
  ChatEnv ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamChat unsafeEnv conv msg callback = do
  let conv' = conv ++ [UserTurn msg]
      env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
      log = onLog (envHooks env)
  log Info $ "streamChat: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv' $ \mc c u ->
    let call req = providerChatStream (mcGateway mc) (envHooks env) req callback
     in chatLoop env mc call 0 u c

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

-- | Shared loop used by both runChat and streamChat.
-- The @call@ parameter abstracts over streaming vs non-streaming.
-- On failure, returns the partial conversation and accumulated usage
-- so that a fallback model can continue from where this one left off.
chatLoop ::
  ChatEnv ->
  ModelConfig ->
  (ChatRequest -> IO LLMResult) ->
  Int ->
  Usage ->
  Conversation ->
  IO (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
chatLoop env mc call rounds acc conv
  | rounds >= envMaxToolRounds env = do
      onLog (envHooks env) Error $ "Tool loop exceeded: " <> T.pack (show rounds) <> " rounds"
      pure $ Left (ToolLoopExceeded rounds, conv, acc)
  | otherwise = do
      aborted <- checkAbort env
      if aborted
        then do
          onLog (envHooks env) Info "Aborted before API call"
          pure $ Left (Aborted, conv, acc)
        else do
          let request = mkRequest env mc conv
              log = onLog (envHooks env)
          log Debug $
            "API request: model="
              <> mcModel mc
              <> " round="
              <> T.pack (show rounds)
              <> " turns="
              <> T.pack (show (length (reqConversation request)))
          case mcThrottleDelay mc of
            Just d -> do
              log Debug $ "Throttle: waiting " <> T.pack (show d) <> "ms"
              threadDelay (d * 1000)
            Nothing -> pure ()
          result <-
            withTimeout (mcRequestTimeout mc) $
              withRetry (mcRetry mc) log $
                call request
          case result of
            Left err -> do
              log Error $ "API error: " <> T.pack (show err)
              pure $ Left (err, conv, acc)
            Right resp ->
              let responseUsage = fromMaybe emptyUsage (respUsage resp)
                  cost = estimateCost (mcPricing mc) responseUsage
                  acc' = addUsage acc (responseUsage {usageTotalCost = cost})
               in if hasToolCalls resp
                    then do
                      let calls = getToolCalls resp
                      log Info $ "Tool calls: " <> T.intercalate ", " (map tcName calls)
                      let offset = windowOffset (envContextWindow env) conv
                          ctx =
                            ToolContext
                              { tcConversation = conv,
                                tcUsage = acc',
                                tcWindowOffset = offset,
                                tcAbortSignal = envAbortSignal env
                              }
                      toolResults <- executeToolsWithAbort (envAbortSignal env) ctx (envTools env) calls
                      case toolResults of
                        Left _ -> do
                          log Info "Aborted during tool execution"
                          pure $ Left (Aborted, conv, acc')
                        Right results -> do
                          log Debug $
                            "Tool results: "
                              <> T.intercalate
                                ", "
                                [trName r <> "=" <> T.take 100 (trContent r) | r <- results]
                          let conv' =
                                conv
                                  ++ [AssistantTurn (respText resp) calls]
                                  ++ [ToolTurn results]
                          chatLoop env mc call (rounds + 1) acc' conv'
                    else do
                      log Info $
                        "Response: "
                          <> T.take 100 (respText resp)
                          <> maybe
                            ""
                            ( \u ->
                                " usage="
                                  <> T.pack (show (usageInputTokens u))
                                  <> "+"
                                  <> T.pack (show (usageOutputTokens u))
                            )
                            (respUsage resp)
                      let finalConv =
                            conv
                              ++ [AssistantTurn (respText resp) []]
                      pure $ Right (respText resp, finalConv, acc')

-- | Build a ChatRequest from the model config and a conversation.
-- When 'envContextWindow' is set, only the last N user messages (and their
-- associated replies) are sent to the model.
mkRequest :: ChatEnv -> ModelConfig -> Conversation -> ChatRequest

-- | Check whether the abort signal has been fired.
checkAbort :: ChatEnv -> IO Bool
checkAbort env = case envAbortSignal env of
  Nothing -> pure False
  Just sig -> isAborted sig

mkRequest env mc conv =
  ChatRequest
    { reqModel = mcModel mc,
      reqConversation = drop offset conv,
      reqSystem = envSystem env,
      reqMaxTokens = mcMaxTokens mc,
      reqTemperature = mcTemperature mc,
      reqTools = map toolDef (envTools env)
    }
  where
    offset = windowOffset (envContextWindow env) conv

-- | Compute the index where the visible window starts.
-- The window includes the last @n@ user messages and all turns that follow
-- each of them (assistant replies, tool rounds, etc.).
-- Returns 0 (no windowing) when the window is 'Nothing' or the conversation
-- contains fewer than @n@ user messages.
windowOffset :: Maybe Int -> Conversation -> Int
windowOffset Nothing _ = 0
windowOffset (Just n) conv = findNthUserFromEnd n conv

-- | Find the index of the Nth 'UserTurn' from the end of a conversation.
-- Returns 0 if there are fewer than @n@ user messages.
findNthUserFromEnd :: Int -> Conversation -> Int
findNthUserFromEnd n conv = go (length conv - 1) n
  where
    go idx remaining
      | idx < 0 = 0
      | remaining <= 0 = idx + 1
      | otherwise = case conv !! idx of
          UserTurn _ -> go (idx - 1) (remaining - 1)
          _ -> go (idx - 1) remaining

-- | Wrap an action with a timeout (ms). Returns 'TimeoutError' on expiry.
withTimeout :: Maybe Int -> IO LLMResult -> IO LLMResult
withTimeout Nothing action = action
withTimeout (Just us) action = do
  result <- timeout (us * 1000) action
  pure $ fromMaybe (Left TimeoutError) result

-- | Retry an action using the retry package's policy (exponential backoff + jitter).
-- The policy controls max attempts, delays, and jitter.
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
