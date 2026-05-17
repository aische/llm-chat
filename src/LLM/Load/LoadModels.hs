module LLM.Load.LoadModels where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT, liftEither)
import Control.Monad.IO.Unlift (MonadIO)
import Data.Map qualified as Map
import LLM.Generate.Types (ModelConfig (..))
import LLM.Load.Types
  ( GatewayMap,
    LoadEnvError (..),
    ModelCatalogItem (..),
    ModelConfigMap,
  )
import LLM.Load.Utils (decodeJsonFile)

loadModelConfigMap :: (MonadIO m) => FilePath -> GatewayMap -> ExceptT LoadEnvError m ModelConfigMap
loadModelConfigMap filePath gatewayMap = do
  modelCatalogItems <- decodeJsonFile filePath LoadModelConfigError
  liftEither $ createModelConfigMap gatewayMap modelCatalogItems

createModelConfigMap :: GatewayMap -> [ModelCatalogItem] -> Either LoadEnvError ModelConfigMap
createModelConfigMap gatewayMap modelCatalogItems = Map.fromList <$> configs
  where
    configs = forM modelCatalogItems $ \mci -> do
      mc <- createModelConfigFromCatalogItem gatewayMap mci
      pure (modelConfigName mci, mc)

createModelConfigFromCatalogItem :: GatewayMap -> ModelCatalogItem -> Either LoadEnvError ModelConfig
createModelConfigFromCatalogItem gatewayMap mci = case Map.lookup (providerName mci) gatewayMap of
  Just gateway ->
    Right
      ModelConfig
        { mcGateway = gateway,
          mcModel = modelName mci,
          mcPricing = pricing mci,
          mcMaxTokens = maxTokens mci,
          mcTemperature = temperature mci,
          mcRequestTimeout = requestTimeout mci,
          mcThrottleDelay = throttleDelay mci,
          mcRetryCount = retryCount mci,
          mcJitterBackoff = jitterBackoff mci
        }
  Nothing -> Left $ LoadProviderError $ "Provider not found: " ++ show (providerName mci)
