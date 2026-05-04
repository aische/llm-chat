module Tools.Readfile (readfileTool) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Core.Types
import Tools.FsConfig

readfileTool :: FsConfig -> Tool
readfileTool cfg =
  Tool
    { toolDef =
        ToolDef
          { toolName = "read_file",
            toolDescription =
              "Read the contents of a file at the given path (relative to the workspace). "
                <> "Returns the full file content as text.",
            toolParameters = readfileSchema
          },
      toolExecute = const (readfileExec cfg)
    }

readfileSchema :: Value
readfileSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "path"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Relative file path to read" :: Text)
                ]
          ],
      "required" .= (["path"] :: [Text])
    ]

readfileExec :: FsConfig -> Value -> IO Text
readfileExec cfg args = do
  let mpath = parseMaybe (withObject "args" (.: "path")) args :: Maybe Text
  case mpath of
    Nothing -> pure "Error: missing 'path' argument"
    Just p -> do
      resolved <- sandboxPath cfg (T.unpack p)
      TIO.readFile resolved
