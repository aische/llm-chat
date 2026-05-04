module Tools.Writefile (writefileTool) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Core.Types
import Tools.FsConfig

writefileTool :: FsConfig -> Tool
writefileTool cfg =
  Tool
    { toolDef =
        ToolDef
          { toolName = "write_file",
            toolDescription =
              "Write content to a file at the given path (relative to the workspace). "
                <> "Creates the file if it doesn't exist, overwrites if it does. "
                <> "Automatically creates parent directories as needed.",
            toolParameters = writefileSchema
          },
      toolExecute = const (writefileExec cfg)
    }

writefileSchema :: Value
writefileSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "path"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Relative file path to write to" :: Text)
                ],
            "content"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("The text content to write to the file" :: Text)
                ]
          ],
      "required" .= (["path", "content"] :: [Text])
    ]

writefileExec :: FsConfig -> Value -> IO Text
writefileExec cfg args = do
  let parsed = flip parseMaybe args $ withObject "args" $ \o -> do
        p <- o .: "path"
        c <- o .: "content"
        pure (p :: Text, c :: Text)
  case parsed of
    Nothing -> pure "Error: missing 'path' or 'content' argument"
    Just (p, content) -> do
      resolved <- sandboxWritePath cfg (T.unpack p)
      TIO.writeFile resolved content
      pure $ "Successfully wrote " <> T.pack (show (T.length content)) <> " characters to " <> p
