module Example3 where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import LLM.Core.Logger
  ( LogLevel (Debug),
    noHooks,
    withJsonDump,
    withStderrLogger,
  )
import LLM.Core.Usage (Usage)
import LLM.Core.Utils (emptyConversation)
import LLM.Generate.GenerateObject (generateObject)
import LLM.Generate.Types (ChatEnv (..), GeneratedResult)
import LLM.Load.LoadEnvs (defaultEnvFilePaths, loadDefaultEnvOrThrow)

data ExampleObject = ExampleObject
  { _title :: Text,
    _content :: Text,
    _rating :: Int,
    _flag :: Bool
  }
  deriving (Show, Generic)
  deriving (FromJSON) via (AC.Autodocodec ExampleObject)

instance AC.HasCodec ExampleObject where
  codec :: AC.JSONCodec ExampleObject
  codec =
    AC.object "ExampleObject" $
      ExampleObject
        <$> AC.requiredField "title" "title of the example" AC..= _title
        <*> AC.requiredField "content" "content of the example" AC..= _content
        <*> AC.requiredField "rating" "quality of the example (1..10)" AC..= _rating
        <*> AC.requiredField "flag" "is the example good?" AC..= _flag

generateExample :: ChatEnv IO -> Text -> IO (GeneratedResult (ExampleObject, Usage))
generateExample env = generateObject env emptyConversation

example :: ChatEnv IO -> IO ()
example env = do
  x <- generateExample env "createn an example of a poem"
  print x

main :: IO ()
main = do
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
  (env, _) <- loadDefaultEnvOrThrow defaultEnvFilePaths hooks
  example env
