module LLM.Load.Utils where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Load.Types (LoadEnvError)

decodeJsonFile ::
  (FromJSON a, MonadIO m) =>
  FilePath ->
  (String -> LoadEnvError) ->
  ExceptT LoadEnvError m a
decodeJsonFile filePath tag =
  ExceptT $
    either (Left . tag) Right
      <$> liftIO (eitherDecodeFileStrict filePath)

getSystemPrompt :: Maybe Text -> ExceptT LoadEnvError IO (Maybe Text)
getSystemPrompt mbSystemPrompt =
  case mbSystemPrompt of
    Nothing -> pure Nothing
    Just t -> do
      if T.isPrefixOf "file:" t
        then
          let filePath = T.drop 5 t
           in liftIO $ fmap Just $ TIO.readFile $ T.unpack filePath
        else pure $ Just t
