module LLM.Tools.Worker where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLM.Core.Types (ToolContext, TypedTool (..))
import LLM.Core.Utils (emptyConversation)
import LLM.Generate.Generate (generateText)
import LLM.Generate.Types (ChatEnv)

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

workerToolTyped :: ChatEnv -> Text -> Text -> TypedTool WorkerToolArgs
workerToolTyped env name description =
  TypedTool
    { ttoolName = name,
      ttoolDescription = description,
      ttoolReadonly = False,
      ttoolExecute = workerExecTyped env
    }

workerExecTyped :: ChatEnv -> ToolContext -> WorkerToolArgs -> IO Text
workerExecTyped env _ctx args = do
  result <- generateText env emptyConversation (_workerPrompt args)
  case result of
    Left e -> pure $ "Error: " <> T.pack (show e)
    Right (answer, _, _) -> pure answer
