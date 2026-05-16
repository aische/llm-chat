module LLM.Load.LoadWorkers where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT (ExceptT), liftEither)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Map qualified as Map
import Data.Text qualified as T
import LLM.Generate.Types
  ( Worker (Worker),
    WorkerMap,
  )
import LLM.Load.Types
  ( ChatEnvMap,
    LoadEnvError (..),
    WorkerConfigItem (..),
  )

loadWorkerMap :: ChatEnvMap -> FilePath -> ExceptT LoadEnvError IO WorkerMap
loadWorkerMap chatEnvMap filePath = do
  workerCatalogItems <-
    ExceptT $
      either (Left . LoadWorkerConfigError) Right
        <$> eitherDecodeFileStrict filePath
  liftEither $ createWorkerMap chatEnvMap workerCatalogItems

createWorkerMap :: ChatEnvMap -> [WorkerConfigItem] -> Either LoadEnvError WorkerMap
createWorkerMap chatEnvMap workerCatalogItems = Map.fromList <$> configs
  where
    configs = forM workerCatalogItems $ \wci -> do
      case Map.lookup (env wci) chatEnvMap of
        Nothing ->
          Left $
            LoadWorkerEnvError
              ( "env "
                  <> T.unpack (env wci)
                  <> " missing for worker"
                  <> T.unpack (name wci)
              )
        Just env ->
          pure
            ( name wci,
              Worker
                (name wci)
                env
                (description wci)
            )
