module LLM.Generate.WorkerTool where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (ToolContext, TypedTool (..))
import LLM.Core.Utils (emptyConversation)
import LLM.Generate.Types (ChatEnv, GenerateText)

newtype WorkerToolArgs = WorkerToolArgs
  { _workerPrompt :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec WorkerToolArgs)

instance AC.HasCodec WorkerToolArgs where
  codec :: AC.JSONCodec WorkerToolArgs
  codec =
    AC.object "WorkerToolArgs" $
      WorkerToolArgs <$> AC.requiredField "prompt" "Prompt to send to the worker" AC..= _workerPrompt

workerToolTyped :: GenerateText -> ChatEnv -> Text -> Text -> TypedTool WorkerToolArgs
workerToolTyped gen env name description =
  TypedTool
    { ttoolName = name,
      ttoolDescription = description,
      ttoolReadonly = False,
      ttoolExecute = workerExecTyped gen env
    }

workerExecTyped :: GenerateText -> ChatEnv -> ToolContext -> WorkerToolArgs -> IO Text
workerExecTyped gen env _ctx args = do
  result <- gen env emptyConversation (_workerPrompt args)
  case result of
    Left e -> pure $ "Error: " <> T.pack (show e)
    Right (answer, _, _) -> pure answer
