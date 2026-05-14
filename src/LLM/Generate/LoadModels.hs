module LLM.Generate.LoadModels where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Retry (fullJitterBackoff, limitRetries)
import Data.Aeson (FromJSON, ToJSON, decode', decodeFileStrict, eitherDecodeFileStrict)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM (ChatEnv (..), claudeGateway, geminiGateway, noHooks, ollamaGateway, openAIGateway)
import LLM.Core.Types (LLMGateway)
import LLM.Core.Usage (PricingInfo (..))
import LLM.Generate.Types (ModelConfig (..))
import System.Environment (lookupEnv)

modelCatalogFilePath :: FilePath
modelCatalogFilePath = "model-catalog.json"

chatEnvCatalogFilePath :: FilePath
chatEnvCatalogFilePath = "chat-env-catalog.json"

data ModelCatalogItem = ModelCatalogItem
  { modelConfigName :: Text,
    providerName :: Text, -- provider name: "openai", "claude", "gemini", "ollama"
    modelName :: Text,
    pricing :: PricingInfo,
    maxTokens :: Int,
    temperature :: Maybe Double,
    requestTimeout :: Maybe Int,
    throttleDelay :: Maybe Int,
    retryCount :: Int,
    jitterBackoff :: Int
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data ChatEnvConfigItem = ChatEnvConfigItem
  { chatEnvName :: Text,
    model :: Text,
    systemPrompt :: Maybe Text,
    fallbacks :: [Text],
    tools :: [Text],
    maximumToolRounds :: Int,
    contextWindowSize :: Maybe Int
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

type GatewayMap = Map Text LLMGateway

type ModelConfigMap = Map Text ModelConfig

type ChatEnvMap = Map Text ChatEnv

loadGateways :: IO GatewayMap
loadGateways = do
  let ollama = Just ("ollama", ollamaGateway)
  openai <- lookupEnv "OPENAI_API_KEY" <&> fmap (("openai",) . openAIGateway . T.pack)
  claude <- lookupEnv "CLAUDE_API_KEY" <&> fmap (("claude",) . claudeGateway . T.pack)
  gemini <- lookupEnv "GEMINI_API_KEY" <&> fmap (("gemini",) . geminiGateway . T.pack)
  pure $ Map.fromList $ catMaybes [openai, claude, gemini, ollama]

getModelConfigs :: GatewayMap -> [ModelCatalogItem] -> Either String ModelConfigMap
getModelConfigs gatewayMap modelCatalogItems = Map.fromList <$> configs
  where
    configs = forM modelCatalogItems $ \mci -> do
      mc <- createModelConfig gatewayMap mci
      pure (modelConfigName mci, mc)

loadModelConfigs :: FilePath -> GatewayMap -> ExceptT String IO ModelConfigMap
loadModelConfigs filePath gatewayMap = do
  modelCatalogItems <- ExceptT $ eitherDecodeFileStrict filePath
  liftEither $ getModelConfigs gatewayMap modelCatalogItems

createModelConfig :: GatewayMap -> ModelCatalogItem -> Either String ModelConfig
createModelConfig gatewayMap mci = case Map.lookup (providerName mci) gatewayMap of
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
  Nothing -> Left $ "Provider not found: " ++ show (providerName mci)

loadChatEnvs :: FilePath -> ModelConfigMap -> ExceptT String IO ChatEnvMap
loadChatEnvs filePath modelConfigMap = do
  chatEnvCatalogItems <- ExceptT $ eitherDecodeFileStrict filePath
  liftEither $ getChatEnvConfigs modelConfigMap chatEnvCatalogItems

getChatEnvConfigs :: ModelConfigMap -> [ChatEnvConfigItem] -> Either String ChatEnvMap
getChatEnvConfigs modelConfigMap chatEnvCatalogItems = Map.fromList <$> configs
  where
    configs = forM chatEnvCatalogItems $ \ceci -> do
      ce <- createChatEnv modelConfigMap ceci
      pure (chatEnvName ceci, ce)

createChatEnv :: ModelConfigMap -> ChatEnvConfigItem -> Either String ChatEnv
createChatEnv models conf = do
  let getModel name = case Map.lookup name models of
        Just mc -> Right mc
        Nothing -> Left $ "Model config not found: " ++ show name
  modelConfig <- getModel (model conf)
  fb <- mapM getModel (fallbacks conf)
  pure $
    ChatEnv
      { envModel = modelConfig,
        envFallbacks = fb,
        envSystem = Nothing,
        envTools = [], -- tools ceci,
        envMaxToolRounds = maximumToolRounds conf,
        envContextWindow = contextWindowSize conf,
        envHooks = noHooks,
        envAbortSignal = Nothing
      }

-- loadEnvs :: ExceptT String IO (ChatEnvMap, ModelConfigMap, GatewayMap)
loadEnvs :: IO (Either String (ChatEnvMap, ModelConfigMap, GatewayMap))
loadEnvs = runExceptT $ do
  gateways <- liftIO loadGateways
  modelConfigs <- loadModelConfigs modelCatalogFilePath gateways
  chatEnvs <- loadChatEnvs chatEnvCatalogFilePath modelConfigs
  pure (chatEnvs, modelConfigs, gateways)
