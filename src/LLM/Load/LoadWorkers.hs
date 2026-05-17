module LLM.Load.LoadWorkers where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT, liftEither)
import Control.Monad.IO.Unlift (MonadIO)
import Data.Map qualified as Map
import Data.Text qualified as T
import LLM.Generate.Types
  ( Worker (..),
    WorkerMap,
  )
import LLM.Load.Types
  ( ChatEnvMap,
    LoadEnvError (..),
    WorkerConfigItem (..),
  )
import LLM.Load.Utils (decodeJsonFile)

loadWorkerMap :: (MonadIO m) => ChatEnvMap m -> FilePath -> ExceptT LoadEnvError m (WorkerMap m)
loadWorkerMap chatEnvMap filePath = do
  workerCatalogItems <- decodeJsonFile filePath LoadWorkerConfigError
  liftEither $ createWorkerMap chatEnvMap workerCatalogItems

createWorkerMap :: ChatEnvMap m -> [WorkerConfigItem] -> Either LoadEnvError (WorkerMap m)
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
                { workerName = name wci,
                  workerEnv = env,
                  workerDescription = description wci
                }
            )
