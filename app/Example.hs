{-# OPTIONS_GHC -Wall #-}

module Example where

import AllModels (AllModels (..), getAllModels)
import Autodocodec qualified as AC
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import LLM.Core.Generate (Generatable, GeneratedResult, generateObject)
import LLM.Core.Logger
  ( LogLevel (..),
    noHooks,
    withJsonDump,
    withStderrLogger,
  )
import LLM.Core.Types (ChatEnv (..))
import LLM.Core.Usage (Usage)
import LLM.Core.Utils (createChatEnv, emptyConversation)

data ExampleObject = ExampleObject
  { title :: Text,
    content :: Text,
    rating :: Int,
    flag :: Bool
  }
  deriving (Show, Generic)

instance FromJSON ExampleObject

instance AC.HasCodec ExampleObject where
  codec =
    AC.object "ExampleObject" $
      ExampleObject
        <$> AC.requiredField "title" "title of the example" AC..= title
        <*> AC.requiredField "content" "content of the example" AC..= content
        <*> AC.requiredField "rating" "quality of the example (1..10)" AC..= rating
        <*> AC.requiredField "flag" "is the example good?" AC..= flag

instance Generatable ExampleObject

generateExample :: ChatEnv -> Text -> IO (GeneratedResult (ExampleObject, Usage))
generateExample env = generateObject env emptyConversation

example :: IO ()
example = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  AllModels {claude_haiku_4_5, llama_3_2} <- getAllModels
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
      systemPrompt = "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to."
      env =
        (createChatEnv llama_3_2 systemPrompt [])
          { envHooks = hooks,
            envContextWindow = Just 3,
            envFallbacks = [claude_haiku_4_5]
          }
  x <- generateExample env "createn an example of a poem"
  print x
