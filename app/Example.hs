module Example where

import Autodocodec qualified as AC
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import LLM.Core.Logger
  ( LogLevel (..),
    noHooks,
    withJsonDump,
    withStderrLogger,
  )
import LLM.Core.Usage (Usage)
import LLM.Core.Utils (emptyConversation)
import LLM.Generate.Generate (Generatable, GeneratedResult, generateObject)
import LLM.Generate.Types (ChatEnv (..))
import LLM.Generate.Utils (createChatEnv)

data ExampleObject = ExampleObject
  { title :: Text,
    content :: Text,
    rating :: Int,
    flag :: Bool
  }
  deriving (Show, Generic, FromJSON)

instance AC.HasCodec ExampleObject where
  codec =
    AC.object "ExampleObject" $
      ExampleObject
        <$> AC.requiredField "title" "title of the example" AC..= title
        <*> AC.requiredField "content" "content of the example" AC..= content
        <*> AC.requiredField "rating" "quality of the example (1..10)" AC..= rating
        <*> AC.requiredField "flag" "is the example good?" AC..= flag

generateExample :: ChatEnv -> Text -> IO (GeneratedResult (ExampleObject, Usage))
generateExample env = generateObject env emptyConversation

example :: ChatEnv -> IO ()
example env = do
  x <- generateExample env "createn an example of a poem"
  print x
