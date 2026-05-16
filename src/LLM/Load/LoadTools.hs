module LLM.Load.LoadTools where

import Data.Map qualified as Map
import LLM
  ( Tool (toolDef),
    ToolDef (toolName),
    toTool,
  )
import LLM.Load.Types
  ( ToolMap,
  )
import LLM.Tools.Age (ageTool)
import LLM.Tools.FsConfig (FsConfig, mkFsConfig)
import LLM.Tools.Readdir (readdirToolTyped)
import LLM.Tools.Readfile (readfileToolTyped)
import LLM.Tools.ReplaceInFile (replaceInFileToolTyped)
import LLM.Tools.Weather (weatherToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)
import System.Environment (lookupEnv)

loadToolMap :: IO (ToolMap, Maybe FsConfig)
loadToolMap = do
  userProjectPath <- lookupEnv "USER_PROJECT_PATH"
  fsConfig <- case userProjectPath of
    Nothing -> pure Nothing
    Just p -> Just <$> mkFsConfig p
  pure (getTools fsConfig, fsConfig)

getTools :: Maybe FsConfig -> ToolMap
getTools fsConfig =
  let fsTools =
        maybe
          []
          ( \fsc ->
              [ toTool (readfileToolTyped fsc),
                toTool (writefileToolTyped fsc),
                toTool (readdirToolTyped fsc),
                toTool (replaceInFileToolTyped fsc)
              ]
          )
      otherTools =
        [ toTool weatherToolTyped,
          ageTool
        ]
   in Map.fromList $ map (\t -> (toolName $ toolDef t, t)) (fsTools fsConfig ++ otherTools)
