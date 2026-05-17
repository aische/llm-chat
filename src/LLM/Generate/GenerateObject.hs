module LLM.Generate.GenerateObject
  ( generateObject,
    generateObjectConversation,
    generateObjectConversationUntyped,
    generateObjectUntyped,
  )
where

import Autodocodec qualified as AC
import Autodocodec.Schema (jsonSchemaVia)
import Control.Concurrent (threadDelay)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Logger
  ( Hooks (onLog),
    LogLevel (Debug, Error, Info),
    safeHooks,
  )
import LLM.Core.ProviderUtils (stripBoundsAndComments)
import LLM.Core.Types
  ( Conversation (..),
    LLMError (ParseError),
    LLMGateway (..),
    Turn (UserTurn),
  )
import LLM.Core.Usage (Usage (..), addUsage, emptyUsage, estimateCost)
import LLM.Core.Utils
  ( withConversation,
    withRetry,
    withTimeout,
  )
import LLM.Generate.Common
  ( mkRequest,
    modelRetryPolicy,
  )
import LLM.Generate.Types
  ( ChatEnv (..),
    Generatable,
    GeneratedResult,
    ModelConfig (..),
  )
import LLM.Generate.WithFallback (withFallback)

generateObject ::
  (MonadUnliftIO m, MonadCatch m, Generatable t) =>
  ChatEnv m ->
  Conversation ->
  Text ->
  m (GeneratedResult (t, Usage))
generateObject unsafeEnv conv msg = generateObjectConversationInternal unsafeEnv AC.codec conv'
  where
    conv' = withConversation conv (++ [UserTurn msg])

generateObjectConversation ::
  (MonadUnliftIO m, MonadCatch m, Generatable t) =>
  ChatEnv m ->
  Conversation ->
  m (GeneratedResult (t, Usage))
generateObjectConversation unsafeEnv = generateObjectConversationInternal unsafeEnv AC.codec

generateObjectConversationInternal ::
  (MonadUnliftIO m, MonadCatch m, Generatable t) =>
  ChatEnv m ->
  AC.JSONCodec t ->
  Conversation ->
  m (GeneratedResult (t, Usage))
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
  (MonadUnliftIO m, MonadCatch m) =>
  ChatEnv m ->
  Value ->
  Conversation ->
  Text ->
  m (GeneratedResult (Value, Usage))
generateObjectUntyped unsafeEnv schema conv msg = generateObjectConversationUntyped unsafeEnv schema conv'
  where
    conv' = withConversation conv (++ [UserTurn msg])

generateObjectConversationUntyped ::
  (MonadUnliftIO m, MonadCatch m) =>
  ChatEnv m ->
  Value ->
  Conversation ->
  m (GeneratedResult (Value, Usage))
generateObjectConversationUntyped unsafeEnv schema conv = do
  let env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  liftIO $ onLog (envHooks env) Info $ "generateText: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv $ \mc c u ->
    let call = liftIO . gwGenerateObject (mcGateway mc) (envHooks env) schema
        logIt = onLog (envHooks env)
        request = mkRequest env mc c (envReadonly env)
     in do
          case mcThrottleDelay mc of
            Just d -> liftIO $ do
              logIt Debug $ "Throttle: waiting " <> T.pack (show d) <> "ms"
              threadDelay (d * 1000)
            Nothing -> pure ()
          result <-
            withTimeout (mcRequestTimeout mc) $
              withRetry (modelRetryPolicy mc) logIt $
                call request
          case result of
            Left err -> do
              liftIO $ logIt Error $ "API error: " <> T.pack (show err)
              pure $ Left (err, conv, u)
            Right (value, mbu) ->
              let responseUsage = fromMaybe emptyUsage mbu
                  cost = estimateCost (mcPricing mc) responseUsage
                  u' = addUsage u (responseUsage {usageTotalCost = cost})
               in pure $ Right (value, u')
