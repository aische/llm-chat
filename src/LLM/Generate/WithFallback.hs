module LLM.Generate.WithFallback (withFallback) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import LLM.Core.Logger
  ( Hooks (onLog),
    LogLevel (Info, Warn),
  )
import LLM.Core.Types
  ( Conversation (..),
    LLMError (Aborted, NetworkError),
    LLMGateway (..),
  )
import LLM.Core.Usage (Usage (..), emptyUsage)
import LLM.Generate.Types
  ( ChatEnv (..),
    GeneratedResult,
    ModelConfig (..),
  )

-- | Try each 'ModelConfig' in order. Falls back on retryable errors.
-- On fallback, the next model continues from the partial conversation
-- and accumulated usage of the failed model, rather than starting over.
withFallback ::
  (MonadIO m) =>
  ChatEnv m ->
  Conversation ->
  (ModelConfig -> Conversation -> Usage -> m (GeneratedResult a)) ->
  m (GeneratedResult a)
withFallback env conv tryModel = go (envModel env : envFallbacks env) conv emptyUsage
  where
    go [] c u = pure $ Left (NetworkError "all models failed", c, u)
    go [mc] c u = do
      liftIO $ onLog (envHooks env) Info $ "Using model: " <> mcModel mc <> " via " <> gwName (mcGateway mc)
      result <- tryModel mc c u
      pure $ case result of
        Left (err, c', u') -> Left (err, c', u')
        Right r -> Right r
    go (mc : rest) c u = do
      liftIO $ onLog (envHooks env) Info $ "Trying model: " <> mcModel mc <> " via " <> gwName (mcGateway mc)
      result <- tryModel mc c u
      case result of
        Left (Aborted, c', u') -> pure $ Left (Aborted, c', u')
        Left (err, c', u') -> do
          liftIO $ onLog (envHooks env) Warn $ "Falling back from " <> mcModel mc <> ": " <> T.pack (show err)
          go rest c' u'
        Right r -> pure $ Right r
