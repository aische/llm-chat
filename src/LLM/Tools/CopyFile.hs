module LLM.Tools.CopyFile (copyFileToolTyped) where

import Autodocodec qualified as AC
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxPath, sandboxWritePath)
import System.Directory (copyFile)

data CopyFileToolArgs = CopyFileToolArgs
  { _cfSrc :: Text,
    _cfDst :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec CopyFileToolArgs)

instance AC.HasCodec CopyFileToolArgs where
  codec =
    AC.object "copy source to destination" $
      CopyFileToolArgs
        <$> AC.requiredField "source" "Relative path of the file to copy" AC..= _cfSrc
        <*> AC.requiredField "destination" "Relative destination path (including filename)" AC..= _cfDst

copyFileToolTyped :: (MonadUnliftIO m) => FsConfig -> TypedTool m CopyFileToolArgs
copyFileToolTyped cfg =
  TypedTool
    { ttoolName = "copy_file",
      ttoolDescription =
        "Copy a file from source to destination (both paths relative to the workspace). "
          <> "Overwrites the destination if it already exists. "
          <> "Creates parent directories at the destination as needed.",
      ttoolReadonly = False,
      ttoolExecute = const (copyFileExecTyped cfg)
    }

copyFileExecTyped :: (MonadUnliftIO m) => FsConfig -> CopyFileToolArgs -> m Text
copyFileExecTyped cfg args = liftIO $ do
  let src = _cfSrc args
      dst = _cfDst args
  srcResolved <- sandboxPath cfg (T.unpack src)
  dstResolved <- sandboxWritePath cfg (T.unpack dst)
  copyFile srcResolved dstResolved
  pure $ "Successfully copied " <> src <> " to " <> dst
