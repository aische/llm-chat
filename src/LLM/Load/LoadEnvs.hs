module LLM.Load.LoadEnvs where

import Control.Lens (Each, each, mapMOf)
import Control.Monad (forM, forM_)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Logger (Hooks, noHooks)
import LLM.Core.Types (Tool)
import LLM.Generate.Types (ChatEnv (..), ModelConfig, WorkerMap)
import LLM.Load.LoadGateways (loadGateways)
import LLM.Load.LoadModels (loadModelConfigMap)
import LLM.Load.LoadTools (loadToolMap)
import LLM.Load.LoadWorkers (loadWorkerMap)
import LLM.Load.Types
  ( ChatEnvConfigItem (..),
    ChatEnvMap,
    EnvFilePaths (..),
    LoadEnvError (..),
    LoadedEnvs (..),
    ModelConfigMap,
    ToolMap,
  )
import LLM.Load.Utils (decodeJsonFile, getSystemPrompt)

defaultEnvFilePaths :: EnvFilePaths
defaultEnvFilePaths =
  EnvFilePaths
    { modelCatalogFilePath = "model-catalog.json",
      chatEnvCatalogFilePath = "chat-env-catalog.json",
      workerCatalogFilePath = Just "worker-catalog.json"
    }

loadDefaultEnvOrThrow :: (MonadUnliftIO m) => EnvFilePaths -> Hooks -> m (ChatEnv m, LoadedEnvs m)
loadDefaultEnvOrThrow envFilePaths = loadEnvOrThrow envFilePaths "default"

loadEnvOrThrow :: (MonadUnliftIO m) => EnvFilePaths -> Text -> Hooks -> m (ChatEnv m, LoadedEnvs m)
loadEnvOrThrow envFilePaths name hooks = do
  envs <- either (error . show) id <$> loadEnvs envFilePaths
  case getLoadedChatEnvByName envs hooks name of
    Left err -> error $ show err
    Right env -> pure (env, envs)

loadEnvsOrThrow :: (MonadUnliftIO m, Each s t Text (ChatEnv m)) => EnvFilePaths -> Hooks -> s -> m (t, LoadedEnvs m)
loadEnvsOrThrow envFilePaths hooks names = do
  envs <- either (error . show) id <$> loadEnvs envFilePaths
  case getAllLoadedChatEnvs envs hooks names of
    Left err -> error $ show err
    Right env -> pure (env, envs)

loadEnvs :: (MonadUnliftIO m) => EnvFilePaths -> m (Either LoadEnvError (LoadedEnvs m))
loadEnvs envFilePaths = runExceptT $ do
  gateways <- liftIO loadGateways
  modelConfigs <- loadModelConfigMap (modelCatalogFilePath envFilePaths) gateways
  (toolMap, mbFsConfig) <- liftIO loadToolMap
  chatEnvs <- loadChatEnvMap (chatEnvCatalogFilePath envFilePaths) modelConfigs toolMap
  workerMap <- maybe (pure Nothing) ((<$>) Just . loadWorkerMap chatEnvs) $ workerCatalogFilePath envFilePaths
  liftEither $ validateWorkersExist chatEnvs workerMap
  pure $
    LoadedEnvs
      { chatEnvs = chatEnvs,
        modelConfigs = modelConfigs,
        gateways = gateways,
        toolMap = toolMap,
        workerMap = workerMap,
        fsConf = mbFsConfig
      }

validateWorkersExist :: ChatEnvMap m -> Maybe (WorkerMap m) -> Either LoadEnvError ()
validateWorkersExist _chatEnvs Nothing = pure ()
validateWorkersExist chatEnvs (Just workerMap) =
  forM_ (Map.toList chatEnvs) $ \(name, chatEnv) ->
    case envWorkers chatEnv of
      Nothing -> pure ()
      Just workers ->
        forM_ workers $ \worker -> do
          case Map.lookup worker workerMap of
            Nothing -> Left $ LoadWorkerMissingError ("worker for env " <> T.unpack name <> " missing: " <> T.unpack worker)
            Just _ -> pure ()

getLoadedChatEnvByName :: LoadedEnvs m -> Hooks -> Text -> Either LoadEnvError (ChatEnv m)
getLoadedChatEnvByName loadedEnvs hooks name =
  case Map.lookup name (chatEnvs loadedEnvs) of
    Nothing -> Left $ LoadChatError $ T.unpack name <> " env not found"
    Just env -> pure env {envHooks = hooks}

getAllLoadedChatEnvs :: (Each s t Text (ChatEnv m)) => LoadedEnvs m -> Hooks -> s -> Either LoadEnvError t
getAllLoadedChatEnvs loadedEnvs hooks = mapMOf each (getLoadedChatEnvByName loadedEnvs hooks)

loadChatEnvMap :: (MonadUnliftIO m) => FilePath -> ModelConfigMap -> ToolMap m -> ExceptT LoadEnvError m (ChatEnvMap m)
loadChatEnvMap filePath modelConfigMap toolMap = do
  chatEnvCatalogItems <- decodeJsonFile filePath LoadChatEnvConfigError
  createChatEnvMap modelConfigMap toolMap chatEnvCatalogItems

createChatEnvMap :: (MonadUnliftIO m) => ModelConfigMap -> ToolMap m -> [ChatEnvConfigItem] -> ExceptT LoadEnvError m (ChatEnvMap m)
createChatEnvMap modelConfigMap toolMap chatEnvCatalogItems = do
  let configs = forM chatEnvCatalogItems $ \ceci -> do
        ce <- createChatEnvFromConfigItem modelConfigMap toolMap ceci
        pure (chatEnvName ceci, ce)
  Map.fromList <$> configs

createChatEnvFromConfigItem :: (MonadUnliftIO m) => ModelConfigMap -> ToolMap m -> ChatEnvConfigItem -> ExceptT LoadEnvError m (ChatEnv m)
createChatEnvFromConfigItem models toolMap conf = do
  modelConfig <- liftEither $ getModel (model conf) models
  fb <- liftEither $ mapM (`getModel` models) (fallbacks conf)
  tools <- liftEither $ mapM (`getTool` toolMap) (tools conf)
  system <- getSystemPrompt (systemPrompt conf)
  pure $
    ChatEnv
      { envModel = modelConfig,
        envFallbacks = fb,
        envSystem = system,
        envTools = tools,
        envReadonly = False,
        envMaxToolRounds = maximumToolRounds conf,
        envContextWindow = contextWindowSize conf,
        envHooks = noHooks,
        envWorkers = workers conf,
        envAbortSignal = Nothing
      }

getModel :: Text -> ModelConfigMap -> Either LoadEnvError ModelConfig
getModel name models = case Map.lookup name models of
  Just mc -> Right mc
  Nothing -> Left $ LoadModelError $ "Model config not found: " ++ show name

getTool :: Text -> ToolMap m -> Either LoadEnvError (Tool m)
getTool name toolMap = case Map.lookup name toolMap of
  Just t -> Right t
  Nothing -> Left $ LoadToolError $ "Tool config not found: " ++ show name
