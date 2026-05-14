module LLM.Generate.LoadModels where

import Control.Lens (Each, each, mapMOf, (<&>))
import Control.Monad (forM)
import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM (ChatEnv (..), Hooks, Tool (toolDef), ToolDef (toolName), claudeGateway, geminiGateway, noHooks, ollamaGateway, openAIGateway, toTool)
import LLM.Core.Types (LLMGateway)
import LLM.Core.Usage (PricingInfo (..))
import LLM.Generate.Types (ModelConfig (..))
import LLM.Tools.Age (ageTool)
import LLM.Tools.FsConfig (FsConfig, mkFsConfig)
import LLM.Tools.Readdir (readdirToolTyped)
import LLM.Tools.Readfile (readfileToolTyped)
import LLM.Tools.ReplaceInFile (replaceInFileToolTyped)
import LLM.Tools.Weather (weatherToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)
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

loadChatEnvs :: FilePath -> ModelConfigMap -> ToolMap -> ExceptT String IO ChatEnvMap
loadChatEnvs filePath modelConfigMap toolMap = do
  chatEnvCatalogItems <- ExceptT $ eitherDecodeFileStrict filePath
  getChatEnvConfigs modelConfigMap toolMap chatEnvCatalogItems

getChatEnvConfigs :: ModelConfigMap -> ToolMap -> [ChatEnvConfigItem] -> ExceptT String IO ChatEnvMap
getChatEnvConfigs modelConfigMap toolMap chatEnvCatalogItems = do
  let configs = forM chatEnvCatalogItems $ \ceci -> do
        ce <- createChatEnv modelConfigMap toolMap ceci
        pure (chatEnvName ceci, ce)
  Map.fromList <$> configs

createChatEnv :: ModelConfigMap -> ToolMap -> ChatEnvConfigItem -> ExceptT String IO ChatEnv
createChatEnv models toolMap conf = do
  let getModel name = case Map.lookup name models of
        Just mc -> Right mc
        Nothing -> Left $ "Model config not found: " ++ show name
      getTool name = case Map.lookup name toolMap of
        Just t -> Right t
        Nothing -> Left $ "Tool config not found: " ++ show name
      getSystem :: ExceptT String IO (Maybe Text)
      getSystem = case systemPrompt conf of
        Nothing -> pure Nothing
        Just t ->
          if T.isPrefixOf "file:" t
            then
              let filePath = T.drop 5 t
               in liftIO $ fmap Just $ TIO.readFile $ T.unpack filePath
            else pure $ Just t

  modelConfig <- liftEither $ getModel (model conf)
  fb <- liftEither $ mapM getModel (fallbacks conf)
  tools <- liftEither $ mapM getTool (tools conf)
  system <- getSystem
  pure $
    ChatEnv
      { envModel = modelConfig,
        envFallbacks = fb,
        envSystem = system,
        envTools = tools,
        envMaxToolRounds = maximumToolRounds conf,
        envContextWindow = contextWindowSize conf,
        envHooks = noHooks,
        envAbortSignal = Nothing
      }

data LoadedEnvs = LoadedEnvs
  { chatEnvs :: ChatEnvMap,
    modelConfigs :: ModelConfigMap,
    gateways :: GatewayMap,
    toolMap :: ToolMap,
    fsConf :: Maybe FsConfig
  }

loadEnvs :: IO (Either String LoadedEnvs)
loadEnvs = runExceptT $ do
  gateways <- liftIO loadGateways
  modelConfigs <- loadModelConfigs modelCatalogFilePath gateways
  (toolMap, mbFsConfig) <- liftIO loadTools
  chatEnvs <- loadChatEnvs chatEnvCatalogFilePath modelConfigs toolMap
  pure $ LoadedEnvs chatEnvs modelConfigs gateways toolMap mbFsConfig

getLoadedEnv :: LoadedEnvs -> Hooks -> Text -> Either String ChatEnv
getLoadedEnv loadedEnvs hooks name =
  case Map.lookup name (chatEnvs loadedEnvs) of
    Nothing -> Left (T.unpack name <> " env not found")
    Just env -> pure env {envHooks = hooks}

getLoadedEnvs :: (Each s t Text ChatEnv) => LoadedEnvs -> Hooks -> s -> Either String t
getLoadedEnvs loadedEnvs hooks = mapMOf each (getLoadedEnv loadedEnvs hooks)

loadDefaultEnvOrThrow :: Hooks -> IO ChatEnv
loadDefaultEnvOrThrow hooks = do
  envs <- either error id <$> loadEnvs
  case getLoadedEnv envs hooks "default" of
    Left err -> error err
    Right env -> pure env

loadEnvsOrThrow :: (Each s t Text ChatEnv) => Hooks -> s -> IO t
loadEnvsOrThrow hooks names = do
  envs <- either error id <$> loadEnvs
  case getLoadedEnvs envs hooks names of
    Left err -> error err
    Right env -> pure env

type ToolMap = Map Text Tool

loadTools :: IO (ToolMap, Maybe FsConfig)
loadTools = do
  userProjectPath <- lookupEnv "USER_PROJECT_PATH"
  fsConfig <- case userProjectPath of
    Nothing -> pure Nothing
    Just p -> Just <$> mkFsConfig p
  pure (getTools fsConfig, fsConfig)

getTools :: Maybe FsConfig -> ToolMap
getTools fsConfig =
  let fsTools =
        maybe
          []
          ( \fsc ->
              [ toTool (readfileToolTyped fsc),
                toTool (writefileToolTyped fsc),
                toTool (readdirToolTyped fsc),
                toTool (replaceInFileToolTyped fsc)
              ]
          )
      otherTools =
        [ toTool weatherToolTyped,
          ageTool
        ]
   in Map.fromList $ map (\t -> (toolName $ toolDef t, t)) (fsTools fsConfig ++ otherTools)
