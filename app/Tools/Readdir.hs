module Tools.Readdir (readdirTool) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types
import System.Directory (doesDirectoryExist, listDirectory)
import Tools.FsConfig

readdirTool :: FsConfig -> Tool
readdirTool cfg =
  Tool
    { toolDef =
        ToolDef
          { toolName = "read_dir",
            toolDescription =
              "List the contents of a directory (relative to the workspace). "
                <> "Returns one entry per line. Directories are suffixed with '/'. "
                <> "Use path '.' or omit it to list the workspace root.",
            toolParameters = readdirSchema
          },
      toolExecute = const (readdirExec cfg)
    }

readdirSchema :: Value
readdirSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "path"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Relative directory path to list (default: '.')" :: Text)
                ]
          ],
      "required" .= ([] :: [Text])
    ]

readdirExec :: FsConfig -> Value -> IO Text
readdirExec cfg args = do
  let mpath = parseMaybe (withObject "args" (.: "path")) args :: Maybe Text
      relPath = maybe "." T.unpack mpath
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
