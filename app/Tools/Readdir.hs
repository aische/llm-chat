module Tools.Readdir (readdirToolTyped) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Aeson.Types (parseMaybe)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Core.Utils (toTool)
import System.Directory (doesDirectoryExist, listDirectory)
import Tools.FsConfig (FsConfig, sandboxPath)

newtype ReaddirToolArgs = ReaddirToolArgs
  { path :: Text
  }
  deriving (Generic)

instance FromJSON ReaddirToolArgs

instance AC.HasCodec ReaddirToolArgs where
  codec :: AC.JSONCodec ReaddirToolArgs
  codec =
    AC.object "ReaddirToolArgs" $
      ReaddirToolArgs <$> AC.requiredField "path" "Relative directory path to list" AC..= path

readdirToolTyped :: FsConfig -> TypedTool ReaddirToolArgs
readdirToolTyped fsConfig =
  TypedTool
    { ttoolName = "read_dir",
      ttoolDescription =
        "List the contents of a directory (relative to the workspace). "
          <> "Returns one entry per line. Directories are suffixed with '/'. "
          <> "Use path '.' or omit it to list the workspace root.",
      ttoolExecute = const (readdirExecTyped fsConfig)
    }

readdirExecTyped :: FsConfig -> ReaddirToolArgs -> IO Text
readdirExecTyped cfg args = do
  let relPath = T.unpack $ path args
  resolved <- sandboxPath cfg relPath
  entries <- sort <$> listDirectory resolved
  annotated <- mapM (annotateEntry resolved) entries
  pure $ T.intercalate "\n" annotated

annotateEntry :: FilePath -> FilePath -> IO Text
annotateEntry parent name = do
  isDir <- doesDirectoryExist (parent <> "/" <> name)
  pure $
    if isDir
      then T.pack name <> "/"
      else T.pack name
