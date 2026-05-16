module LLM.Generate.Generate
  ( generateText,
    generateTextConversation,
    streamText,
    streamTextWithWorkers,
    streamTextConversation,
    generateObjectUntyped,
    generateObjectConversationUntyped,
    generateObject,
    generateObjectConversation,
    GeneratedResult,
    Generatable,
  )
where

import Autodocodec qualified as AC
import Autodocodec.Schema (jsonSchemaVia)
import Control.Concurrent (threadDelay)
import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Abort (isAborted)
import LLM.Core.Logger
  ( Hooks (onLog),
    LogLevel (Debug, Error, Info),
    safeHooks,
  )
import LLM.Core.ProviderUtils (stripBoundsAndComments)
import LLM.Core.Types
  ( ChatRequest (..),
    ChatResponse (respText, respUsage),
    Conversation (..),
    LLMError (Aborted, ParseError, ToolLoopExceeded),
    LLMGateway (..),
    LLMTextResult,
    StreamEvent,
    ToolContext (..),
    Turn (AssistantTurn, ToolTurn, UserTurn),
  )
import LLM.Core.Usage (Usage (..), addUsage, emptyUsage, estimateCost)
import LLM.Core.Utils
  ( appendConversation,
    executeToolsWithAbort,
    getToolCalls,
    hasToolCalls,
    withConversation,
    withRetry,
    withTimeout,
  )
import LLM.Generate.Common
  ( getFilteredToolsWithWorkers,
    mkRequest,
    mkRequestWithWorkers,
    modelRetryPolicy,
    requestLogMessage,
    responseLogMessage,
    toolCallsLogMessage,
    toolResultsLogMessage,
    windowOffset,
  )
import LLM.Generate.Types
  ( ChatEnv (..),
    Generatable,
    GenerateText,
    GeneratedResult,
    ModelConfig (..),
    WorkerMap,
  )
import LLM.Generate.WithFallback (withFallback)

-- | Run a non-streaming chat with automatic tool-call handling.
-- Tries each model in 'envModels' in order, falling back on retryable errors.
generateText :: ChatEnv -> Conversation -> Text -> IO (GeneratedResult (Text, Conversation, Usage))
generateText = generateTextWithWorkers Nothing

generateTextWithWorkers ::
  Maybe WorkerMap ->
  ChatEnv ->
  Conversation ->
  Text ->
  IO (GeneratedResult (Text, Conversation, Usage))
generateTextWithWorkers mbWorkerMap unsafeEnv conv msg = generateTextConversation mbGenWorkerMap unsafeEnv conv'
  where
    conv' = withConversation conv (++ [UserTurn msg])
    mbGenWorkerMap = fmap (generateTextWithWorkers mbWorkerMap,) mbWorkerMap

generateTextConversation ::
  Maybe (GenerateText, WorkerMap) ->
  ChatEnv ->
  Conversation ->
  IO (GeneratedResult (Text, Conversation, Usage))
generateTextConversation mbGenWorkerMap unsafeEnv conv = do
  let env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  onLog (envHooks env) Info $ "generateText: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv $ \mc c u ->
    let call = gwGenerateText (mcGateway mc) (envHooks env)
     in chatLoop mbGenWorkerMap env mc call 0 u c

-- | Like 'generateText', but streams text deltas via a callback as they arrive.
streamText :: ChatEnv -> Conversation -> Text -> (StreamEvent -> IO ()) -> IO (GeneratedResult (Text, Conversation, Usage))
streamText = streamTextWithWorkers Nothing

streamTextWithWorkers ::
  Maybe WorkerMap ->
  ChatEnv ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  IO (GeneratedResult (Text, Conversation, Usage))
streamTextWithWorkers mbWorkerMap unsafeEnv conv msg callback = streamTextConversation mbGenWorkerMap unsafeEnv conv' callback
  where
    conv' = withConversation conv (++ [UserTurn msg])
    mbGenWorkerMap = fmap (\c d t -> streamTextWithWorkers mbWorkerMap c d t callback,) mbWorkerMap

streamTextConversation ::
  Maybe (GenerateText, WorkerMap) ->
  ChatEnv ->
  Conversation ->
  (StreamEvent -> IO ()) ->
  IO (GeneratedResult (Text, Conversation, Usage))
streamTextConversation mbGenWorkerMap unsafeEnv conv callback = do
  let env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
      logIt = onLog (envHooks env)
  logIt Info $ "streamText: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv $ \mc c u ->
    let call req = gwStreamText (mcGateway mc) (envHooks env) req callback
     in chatLoop mbGenWorkerMap env mc call 0 u c

generateObject ::
  (Generatable t) =>
  ChatEnv ->
  Conversation ->
  Text ->
  IO (GeneratedResult (t, Usage))
generateObject unsafeEnv conv msg = generateObjectConversationInternal unsafeEnv AC.codec conv'
  where
    conv' = withConversation conv (++ [UserTurn msg])

generateObjectConversation ::
  (Generatable t) =>
  ChatEnv ->
  Conversation ->
  IO (GeneratedResult (t, Usage))
generateObjectConversation unsafeEnv = generateObjectConversationInternal unsafeEnv AC.codec

generateObjectConversationInternal ::
  (Generatable t) =>
  ChatEnv ->
  AC.JSONCodec t ->
  Conversation ->
  IO (GeneratedResult (t, Usage))
generateObjectConversationInternal unsafeEnv codec conv = do
  let jsonschema = stripBoundsAndComments $ AE.toJSON $ jsonSchemaVia codec
  res <- generateObjectConversationUntyped unsafeEnv jsonschema conv
  case res of
    Left (e, conv', u) -> pure (Left (e, conv', u))
    Right (v, u) -> do
      case AE.fromJSON v of
        AE.Error e -> pure $ Left (ParseError $ "Can't decode object returned from generateObjectUntyped" <> T.pack (show e), conv, emptyUsage) -- TODO: e not used
        AE.Success a -> pure $ Right (a, u)

generateObjectUntyped ::
  ChatEnv ->
  Value ->
  Conversation ->
  Text ->
  IO (GeneratedResult (Value, Usage))
generateObjectUntyped unsafeEnv schema conv msg = generateObjectConversationUntyped unsafeEnv schema conv'
  where
    conv' = withConversation conv (++ [UserTurn msg])

generateObjectConversationUntyped ::
  ChatEnv ->
  Value ->
  Conversation ->
  IO (GeneratedResult (Value, Usage))
generateObjectConversationUntyped unsafeEnv schema conv = do
  let env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  onLog (envHooks env) Info $ "generateText: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv $ \mc c u ->
    let call = gwGenerateObject (mcGateway mc) (envHooks env) schema
        logIt = onLog (envHooks env)
        request = mkRequest env mc c (envReadonly env)
     in do
          case mcThrottleDelay mc of
            Just d -> do
              logIt Debug $ "Throttle: waiting " <> T.pack (show d) <> "ms"
              threadDelay (d * 1000)
            Nothing -> pure ()
          result <-
            withTimeout (mcRequestTimeout mc) $
              withRetry (modelRetryPolicy mc) logIt $
                call request
          case result of
            Left err -> do
              logIt Error $ "API error: " <> T.pack (show err)
              pure $ Left (err, conv, u)
            Right (value, mbu) ->
              let responseUsage = fromMaybe emptyUsage mbu
                  cost = estimateCost (mcPricing mc) responseUsage
                  u' = addUsage u (responseUsage {usageTotalCost = cost})
               in pure $ Right (value, u')

-- | Shared loop used by both generateText and streamText.
-- The @call@ parameter abstracts over streaming vs non-streaming.
-- On failure, returns the partial conversation and accumulated usage
-- so that a fallback model can continue from where this one left off.
chatLoop ::
  Maybe (GenerateText, WorkerMap) ->
  ChatEnv ->
  ModelConfig ->
  (ChatRequest -> IO LLMTextResult) ->
  Int ->
  Usage ->
  Conversation ->
  IO (GeneratedResult (Text, Conversation, Usage))
chatLoop mbGenWorkerMap env mc call rounds acc conv
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
          let request = mkRequestWithWorkers mbGenWorkerMap env mc conv (envReadonly env)
              logIt = onLog (envHooks env)
          logIt Debug $ requestLogMessage mc rounds request
          case mcThrottleDelay mc of
            Just d -> do
              logIt Debug $ "Throttle: waiting " <> T.pack (show d) <> "ms"
              threadDelay (d * 1000)
            Nothing -> pure ()
          result <-
            withTimeout (mcRequestTimeout mc) $
              withRetry (modelRetryPolicy mc) logIt $
                call request
          case result of
            Left err -> do
              logIt Error $ "API error: " <> T.pack (show err)
              pure $ Left (err, conv, acc)
            Right resp ->
              let responseUsage = fromMaybe emptyUsage (respUsage resp)
                  cost = estimateCost (mcPricing mc) responseUsage
                  acc' = addUsage acc (responseUsage {usageTotalCost = cost})
               in if hasToolCalls resp
                    then do
                      let calls = getToolCalls resp
                      logIt Info $ toolCallsLogMessage calls
                      let offset = windowOffset (envContextWindow env) conv
                          ctx =
                            ToolContext
                              { tcConversation = conv,
                                tcUsage = acc',
                                tcWindowOffset = offset,
                                tcAbortSignal = envAbortSignal env
                              }
                          filteredTools = getFilteredToolsWithWorkers mbGenWorkerMap False env -- todo: readonly tools
                      toolResults <- executeToolsWithAbort (envAbortSignal env) ctx filteredTools calls
                      case toolResults of
                        Left _ -> do
                          logIt Info "Aborted during tool execution"
                          pure $ Left (Aborted, conv, acc')
                        Right results -> do
                          logIt Debug $ toolResultsLogMessage results
                          let conv' = appendConversation conv [AssistantTurn (respText resp) calls, ToolTurn results]
                          chatLoop mbGenWorkerMap env mc call (rounds + 1) acc' conv'
                    else do
                      logIt Info $ responseLogMessage resp
                      let finalConv = withConversation conv (++ [AssistantTurn (respText resp) []])
                      pure $ Right (respText resp, finalConv, acc')

-- | Check whether the abort signal has been fired.
checkAbort :: ChatEnv -> IO Bool
checkAbort env = case envAbortSignal env of
  Nothing -> pure False
  Just sig -> isAborted sig
