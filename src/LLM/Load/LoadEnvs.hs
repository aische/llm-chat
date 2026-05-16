module LLM.Load.LoadEnvs where

import Control.Lens (Each, each, mapMOf)
import Control.Monad (forM, forM_)
import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Core.Logger (Hooks, noHooks)
import LLM.Generate.Types (ChatEnv (..), WorkerMap)
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

defaultEnvFilePaths :: EnvFilePaths
defaultEnvFilePaths =
  EnvFilePaths
    "model-catalog.json"
    "chat-env-catalog.json"
    (Just "worker-catalog.json")

loadDefaultEnvOrThrow :: EnvFilePaths -> Hooks -> IO (ChatEnv, LoadedEnvs)
loadDefaultEnvOrThrow envFilePaths = loadEnvOrThrow envFilePaths "default"

loadEnvOrThrow :: EnvFilePaths -> Text -> Hooks -> IO (ChatEnv, LoadedEnvs)
loadEnvOrThrow envFilePaths name hooks = do
  envs <- either (error . show) id <$> loadEnvs envFilePaths
  case getLoadedChatEnvByName envs hooks name of
    Left err -> error $ show err
    Right env -> pure (env, envs)

loadEnvsOrThrow :: (Each s t Text ChatEnv) => EnvFilePaths -> Hooks -> s -> IO (t, LoadedEnvs)
loadEnvsOrThrow envFilePaths hooks names = do
  envs <- either (error . show) id <$> loadEnvs envFilePaths
  case getAllLoadedChatEnvs envs hooks names of
    Left err -> error $ show err
    Right env -> pure (env, envs)

loadEnvs :: EnvFilePaths -> IO (Either LoadEnvError LoadedEnvs)
loadEnvs envFilePaths = runExceptT $ do
  gateways <- liftIO loadGateways
  modelConfigs <- loadModelConfigMap (modelCatalogFilePath envFilePaths) gateways
  (toolMap, mbFsConfig) <- liftIO loadToolMap
  chatEnvs <- loadChatEnvMap (chatEnvCatalogFilePath envFilePaths) modelConfigs toolMap
  workerMap <- maybe (pure Nothing) ((<$>) Just . loadWorkerMap chatEnvs) $ workerCatalogFilePath envFilePaths
  liftEither $ validateWorkersExist chatEnvs workerMap
  pure $ LoadedEnvs chatEnvs modelConfigs gateways toolMap workerMap mbFsConfig

validateWorkersExist :: ChatEnvMap -> Maybe WorkerMap -> Either LoadEnvError ()
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

getLoadedChatEnvByName :: LoadedEnvs -> Hooks -> Text -> Either LoadEnvError ChatEnv
getLoadedChatEnvByName loadedEnvs hooks name =
  case Map.lookup name (chatEnvs loadedEnvs) of
    Nothing -> Left $ LoadChatError $ T.unpack name <> " env not found"
    Just env -> pure env {envHooks = hooks}

getAllLoadedChatEnvs :: (Each s t Text ChatEnv) => LoadedEnvs -> Hooks -> s -> Either LoadEnvError t
getAllLoadedChatEnvs loadedEnvs hooks = mapMOf each (getLoadedChatEnvByName loadedEnvs hooks)

loadChatEnvMap :: FilePath -> ModelConfigMap -> ToolMap -> ExceptT LoadEnvError IO ChatEnvMap
loadChatEnvMap filePath modelConfigMap toolMap = do
  chatEnvCatalogItems <-
    ExceptT $
      either (Left . LoadChatEnvConfigError) Right
        <$> eitherDecodeFileStrict filePath
  createChatEnvMap modelConfigMap toolMap chatEnvCatalogItems

createChatEnvMap :: ModelConfigMap -> ToolMap -> [ChatEnvConfigItem] -> ExceptT LoadEnvError IO ChatEnvMap
createChatEnvMap modelConfigMap toolMap chatEnvCatalogItems = do
  let configs = forM chatEnvCatalogItems $ \ceci -> do
        ce <- createChatEnvFromConfigItem modelConfigMap toolMap ceci
        pure (chatEnvName ceci, ce)
  Map.fromList <$> configs

createChatEnvFromConfigItem :: ModelConfigMap -> ToolMap -> ChatEnvConfigItem -> ExceptT LoadEnvError IO ChatEnv
createChatEnvFromConfigItem models toolMap conf = do
  let getModel name = case Map.lookup name models of
        Just mc -> Right mc
        Nothing -> Left $ LoadModelError $ "Model config not found: " ++ show name
      getTool name = case Map.lookup name toolMap of
        Just t -> Right t
        Nothing -> Left $ LoadToolError $ "Tool config not found: " ++ show name
      getSystem :: ExceptT LoadEnvError IO (Maybe Text)
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
        envReadonly = False,
        envMaxToolRounds = maximumToolRounds conf,
        envContextWindow = contextWindowSize conf,
        envHooks = noHooks,
        envWorkers = workers conf,
        envAbortSignal = Nothing
      }
