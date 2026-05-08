module LLM.Core.Generate
  ( generateText,
    generateTextConversation,
    streamText,
    streamTextConversation,
    generateObjectUntyped,
    generateObjectConversationUntyped,
    generateObject,
    generateObjectConversation,
    ModelConfig (..),
    ChatEnv (..),
    defaultChatEnv,
    createChatEnv,
    GeneratedResult,
    Generatable,
  )
where

import Autodocodec (HasCodec)
import Autodocodec qualified as AC
import Autodocodec.Schema (jsonSchemaVia)
import Control.Concurrent (threadDelay)
import Control.Retry (RetryPolicyM)
import Data.Aeson (FromJSON, Value)
import Data.Aeson qualified as AE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Abort (AbortSignal, isAborted)
import LLM.Core.Logger
  ( Hooks (onLog),
    LogLevel (Debug, Error, Info, Warn),
    noHooks,
    safeHooks,
  )
import LLM.Core.ProviderUtils (stripBoundsAndComments)
import LLM.Core.Types
  ( ChatRequest (..),
    ChatResponse (respText, respUsage),
    Conversation (..),
    LLMError (Aborted, NetworkError, ParseError, ToolLoopExceeded),
    LLMGateway (..),
    LLMTextResult,
    StreamEvent,
    Tool (toolDef),
    ToolCall (tcName),
    ToolContext (..),
    ToolResult (trContent, trName),
    Turn (AssistantTurn, ToolTurn, UserTurn),
  )
import LLM.Core.Usage (PricingInfo (..), Usage (..), addUsage, emptyUsage, estimateCost)
import LLM.Core.Utils
  ( executeToolsWithAbort,
    getToolCalls,
    hasToolCalls,
    withConversation,
    withRetry,
    withTimeout,
  )

-- | Infrastructure-level configuration for a specific model.
-- Bundles together everything needed to reach one model endpoint.
-- Use a list of these in 'ChatEnv' for fallback across models/providers.
data ModelConfig = ModelConfig
  { mcGateway :: LLMGateway,
    mcModel :: Text,
    mcPricing :: PricingInfo,
    mcMaxTokens :: Int,
    mcTemperature :: Maybe Double,
    mcRequestTimeout :: Maybe Int, -- milliseconds; timeout the whole request if it takes too long
    mcThrottleDelay :: Maybe Int, -- milliseconds; wait before each API call
    mcRetry :: RetryPolicyM IO
  }

-- | Application-level chat configuration.
-- Tools, system prompt, and hooks stay fixed across fallback attempts.
data ChatEnv = ChatEnv
  { envModel :: ModelConfig, -- primary
    envFallbacks :: [ModelConfig], -- fallbacks, tried in order
    envSystem :: Maybe Text,
    envTools :: [Tool],
    envMaxToolRounds :: Int,
    envContextWindow :: Maybe Int, -- max recent turns sent to the model; Nothing = all
    envHooks :: Hooks,
    envAbortSignal :: Maybe AbortSignal
  }

-- | Sensible defaults — single model, no fallback.
defaultChatEnv :: ModelConfig -> ChatEnv
defaultChatEnv mc =
  ChatEnv
    { envModel = mc,
      envFallbacks = [],
      envSystem = Nothing,
      envTools = [],
      envMaxToolRounds = 10,
      envContextWindow = Nothing,
      envHooks = noHooks,
      envAbortSignal = Nothing
    }

createChatEnv :: ModelConfig -> Text -> [Tool] -> ChatEnv
createChatEnv mc system tools =
  ChatEnv
    { envModel = mc,
      envFallbacks = [],
      envSystem = Just system,
      envTools = tools,
      envMaxToolRounds = 10,
      envContextWindow = Nothing,
      envHooks = noHooks,
      envAbortSignal = Nothing
    }

-- | Run a non-streaming chat with automatic tool-call handling.
-- Tries each model in 'envModels' in order, falling back on retryable errors.
generateText ::
  ChatEnv ->
  Conversation ->
  Text ->
  IO (GeneratedResult (Text, Conversation, Usage))
generateText unsafeEnv conv msg = generateTextConversation unsafeEnv conv'
  where
    conv' = withConversation conv (++ [UserTurn msg])

generateTextConversation ::
  ChatEnv ->
  Conversation ->
  IO (GeneratedResult (Text, Conversation, Usage))
generateTextConversation unsafeEnv conv = do
  let env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  onLog (envHooks env) Info $ "generateText: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv $ \mc c u ->
    let call = gwGenerateText (mcGateway mc) (envHooks env)
     in chatLoop env mc call 0 u c

-- | Like 'generateText', but streams text deltas via a callback as they arrive.
streamText ::
  ChatEnv ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  IO (GeneratedResult (Text, Conversation, Usage))
streamText unsafeEnv conv msg = streamTextConversation unsafeEnv conv'
  where
    conv' = withConversation conv (++ [UserTurn msg])

streamTextConversation ::
  ChatEnv ->
  Conversation ->
  (StreamEvent -> IO ()) ->
  IO (GeneratedResult (Text, Conversation, Usage))
streamTextConversation unsafeEnv conv callback = do
  let env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
      logIt = onLog (envHooks env)
  logIt Info $ "streamText: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv $ \mc c u ->
    let call req = gwStreamText (mcGateway mc) (envHooks env) req callback
     in chatLoop env mc call 0 u c

type GeneratedResult a = Either (LLMError, Conversation, Usage) a

class (HasCodec t, FromJSON t) => Generatable t

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
        AE.Error _e -> pure $ Left (ParseError "Can't decode object returned from generateObjectUntyped", conv, emptyUsage) -- TODO: e not used
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
        request = mkRequest env mc c
     in do
          case mcThrottleDelay mc of
            Just d -> do
              logIt Debug $ "Throttle: waiting " <> T.pack (show d) <> "ms"
              threadDelay (d * 1000)
            Nothing -> pure ()
          result <-
            withTimeout (mcRequestTimeout mc) $
              withRetry (mcRetry mc) logIt $
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

-- | Try each 'ModelConfig' in order. Falls back on retryable errors.
-- On fallback, the next model continues from the partial conversation
-- and accumulated usage of the failed model, rather than starting over.
withFallback ::
  ChatEnv ->
  Conversation ->
  (ModelConfig -> Conversation -> Usage -> IO (GeneratedResult a)) ->
  IO (GeneratedResult a)
-- withFallback ::
--   ChatEnv ->
--   Conversation ->
--   (ModelConfig -> Conversation -> Usage -> IO (GeneratedResult (Text, Conversation, Usage))) ->
--   IO (GeneratedResult (Text, Conversation, Usage))
withFallback env conv tryModel = go (envModel env : envFallbacks env) conv emptyUsage
  where
    go [] c u = pure $ Left (NetworkError "all models failed", c, u)
    go [mc] c u = do
      onLog (envHooks env) Info $ "Using model: " <> mcModel mc <> " via " <> gwName (mcGateway mc)
      result <- tryModel mc c u
      pure $ case result of
        Left (err, c', u') -> Left (err, c', u')
        Right r -> Right r
    go (mc : rest) c u = do
      onLog (envHooks env) Info $ "Trying model: " <> mcModel mc <> " via " <> gwName (mcGateway mc)
      result <- tryModel mc c u
      case result of
        Left (Aborted, c', u') -> pure $ Left (Aborted, c', u')
        Left (err, c', u') -> do
          onLog (envHooks env) Warn $ "Falling back from " <> mcModel mc <> ": " <> T.pack (show err)
          go rest c' u'
        Right r -> pure $ Right r

-- | Shared loop used by both generateText and streamText.
-- The @call@ parameter abstracts over streaming vs non-streaming.
-- On failure, returns the partial conversation and accumulated usage
-- so that a fallback model can continue from where this one left off.
chatLoop ::
  ChatEnv ->
  ModelConfig ->
  (ChatRequest -> IO LLMTextResult) ->
  Int ->
  Usage ->
  Conversation ->
  IO (GeneratedResult (Text, Conversation, Usage))
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
              logIt = onLog (envHooks env)
          logIt Debug $
            "API request: model="
              <> mcModel mc
              <> " round="
              <> T.pack (show rounds)
              <> " turns="
              <> T.pack (show (length (unConversation (reqConversation request))))
          case mcThrottleDelay mc of
            Just d -> do
              logIt Debug $ "Throttle: waiting " <> T.pack (show d) <> "ms"
              threadDelay (d * 1000)
            Nothing -> pure ()
          result <-
            withTimeout (mcRequestTimeout mc) $
              withRetry (mcRetry mc) logIt $
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
                      logIt Info $ "Tool calls: " <> T.intercalate ", " (map tcName calls)
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
                          logIt Info "Aborted during tool execution"
                          pure $ Left (Aborted, conv, acc')
                        Right results -> do
                          logIt Debug $
                            "Tool results: "
                              <> T.intercalate
                                ", "
                                [trName r <> "=" <> T.take 100 (trContent r) | r <- results]
                          let conv' =
                                withConversation
                                  conv
                                  (++ [AssistantTurn (respText resp) calls, ToolTurn results])
                          chatLoop env mc call (rounds + 1) acc' conv'
                    else do
                      logIt Info $
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
                            withConversation conv (++ [AssistantTurn (respText resp) []])
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
      reqConversation = withConversation conv (drop offset),
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
findNthUserFromEnd n conv = go (length (unConversation conv) - 1) n
  where
    go idx remaining
      | idx < 0 = 0
      | remaining <= 0 = idx + 1
      | otherwise = case unConversation conv !! idx of
          UserTurn _ -> go (idx - 1) (remaining - 1)
          _ -> go (idx - 1) remaining
