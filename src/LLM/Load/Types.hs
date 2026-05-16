module LLM.Load.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import LLM.Core.Types (LLMGateway, Tool)
import LLM.Core.Usage (PricingInfo)
import LLM.Generate.Types (ChatEnv, ModelConfig)
import LLM.Tools.FsConfig (FsConfig)

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
    workers :: Maybe [Text],
    maximumToolRounds :: Int,
    contextWindowSize :: Maybe Int
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data WorkerConfigItem = WorkerConfigItem
  { workerName :: Text,
    workerEnv :: Text,
    description :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

type WorkerMap = Map Text WorkerConfigItem

type GatewayMap = Map Text LLMGateway

type ModelConfigMap = Map Text ModelConfig

type ChatEnvMap = Map Text ChatEnv

type ToolMap = Map Text Tool

data LoadedEnvs = LoadedEnvs
  { chatEnvs :: ChatEnvMap,
    modelConfigs :: ModelConfigMap,
    gateways :: GatewayMap,
    toolMap :: ToolMap,
    workerMap :: Maybe WorkerMap,
    fsConf :: Maybe FsConfig
  }

data LoadEnvError
  = LoadModelConfigError String
  | LoadProviderError String
  | LoadChatEnvConfigError String
  | LoadModelError String
  | LoadToolError String
  | LoadChatError String
  | LoadWorkerConfigError String
  | LoadWorkerEnvError String
  | LoadWorkerMissingError String
  deriving (Show)

data EnvFilePaths = EnvFilePaths
  { modelCatalogFilePath :: FilePath,
    chatEnvCatalogFilePath :: FilePath,
    workerCatalogFilePath :: Maybe FilePath
  }