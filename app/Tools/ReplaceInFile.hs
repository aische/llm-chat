module Tools.ReplaceInFile (replaceInFileTool) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Core.Types
import Tools.FsConfig

replaceInFileTool :: FsConfig -> Tool
replaceInFileTool cfg =
  Tool
    { toolDef =
        ToolDef
          { toolName = "replace_in_file",
            toolDescription =
              "Replace the first occurrence of a string in a file. "
                <> "The 'old' string must appear exactly once in the file. "
                <> "Returns an error if the string is not found or appears more than once.",
            toolParameters = replaceSchema
          },
      toolExecute = const (replaceExec cfg)
    }

replaceSchema :: Value
replaceSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "path"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Relative file path to modify" :: Text)
                ],
            "old"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("The exact text to find (must appear exactly once)" :: Text)
                ],
            "new"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("The replacement text" :: Text)
                ]
          ],
      "required" .= (["path", "old", "new"] :: [Text])
    ]

replaceExec :: FsConfig -> Value -> IO Text
replaceExec cfg args = do
  let parsed = flip parseMaybe args $ withObject "args" $ \o -> do
        p <- o .: "path"
        old <- o .: "old"
        new <- o .: "new"
        pure (p :: Text, old :: Text, new :: Text)
  case parsed of
    Nothing -> pure "Error: missing 'path', 'old', or 'new' argument"
    Just (p, old, new) -> do
      resolved <- sandboxPath cfg (T.unpack p)
      content <- TIO.readFile resolved
      let occurrences = countOccurrences old content
      case occurrences of
        0 -> pure "Error: the 'old' string was not found in the file"
        1 -> do
          let replaced = replaceFirst old new content
          TIO.writeFile resolved replaced
          pure $ "Successfully replaced text in " <> p
        n ->
          pure $
            "Error: the 'old' string was found "
              <> T.pack (show n)
              <> " times; it must appear exactly once"

-- | Count non-overlapping occurrences of needle in haystack.
countOccurrences :: Text -> Text -> Int
countOccurrences needle haystack
  | T.null needle = 0
  | otherwise = go 0 haystack
  where
    go !n hay = case T.breakOn needle hay of
      (_, rest)
        | T.null rest -> n
        | otherwise -> go (n + 1) (T.drop (T.length needle) rest)

-- | Replace the first occurrence of needle with replacement.
replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle replacement haystack =
  let (before, after) = T.breakOn needle haystack
   in before <> replacement <> T.drop (T.length needle) after
