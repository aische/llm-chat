module Tools.Readfile (readfileToolTyped) where

import Autodocodec qualified as AC
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM.Core.Types
import Tools.FsConfig

newtype ReadfileToolArgs = ReadfileToolArgs
  { path :: Text
  }
  deriving (Generic)

instance FromJSON ReadfileToolArgs

instance AC.HasCodec ReadfileToolArgs where
  codec =
    AC.object "ReadfileToolArgs" $
      ReadfileToolArgs <$> AC.requiredField "path" "Relative file path to read" AC..= path

readfileToolTyped :: FsConfig -> TypedTool ReadfileToolArgs
readfileToolTyped cfg =
  TypedTool
    { ttoolName = "read_file",
      ttoolDescription =
        "Read the contents of a file at the given path (relative to the workspace). "
          <> "Returns the full file content as text.",
      ttoolExecute = const (readfileExecTyped cfg)
    }

readfileExecTyped :: FsConfig -> ReadfileToolArgs -> IO Text
readfileExecTyped cfg args = do
  let p = path args
  resolved <- sandboxPath cfg (T.unpack p)
  TIO.readFile resolved
