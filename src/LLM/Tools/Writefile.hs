module LLM.Tools.Writefile (writefileToolTyped) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))
import LLM.Tools.FsConfig (FsConfig, sandboxWritePath)

data WritefileToolArgs = WritefileToolArgs
  { path :: Text,
    content :: Text
  }
  deriving (Generic)
  deriving anyclass (FromJSON)

instance AC.HasCodec WritefileToolArgs where
  codec =
    AC.object "WritefileToolArgs" $
      WritefileToolArgs
        <$> AC.requiredField "path" "Relative file path to write to" AC..= path
        <*> AC.requiredField "content" "The text content to write to the file" AC..= content

writefileToolTyped :: FsConfig -> TypedTool WritefileToolArgs
writefileToolTyped cfg =
  TypedTool
    { ttoolName = "write_file",
      ttoolDescription =
        "Write content to a file at the given path (relative to the workspace). "
          <> "Creates the file if it doesn't exist, overwrites if it does. "
          <> "Automatically creates parent directories as needed.",
      ttoolExecute = const (writefileExecTyped cfg)
    }

writefileExecTyped :: FsConfig -> WritefileToolArgs -> IO Text
writefileExecTyped cfg args = do
  let p = path args
      c = content args
  resolved <- sandboxWritePath cfg (T.unpack p)
  TIO.writeFile resolved c
  pure $ "Successfully wrote " <> T.pack (show (T.length c)) <> " characters to " <> p
