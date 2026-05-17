module Example6 where

import Adapters.Repl (repl)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import LLM.Core.Logger (defaultDebugHooks)
import LLM.Core.Utils (emptyConversation)
import LLM.Generate.Generate (generateText)
import LLM.Load.LoadEnvs (defaultEnvFilePaths, loadDefaultEnvOrThrow)
import UnliftIO (MonadUnliftIO)

data MyState = MyState
  { envMaxTimeout :: Int,
    envLogPrefix :: String
  }

newtype App m a = App {runApp :: ReaderT MyState m a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadIO, MonadReader MyState, MonadUnliftIO)

main :: IO ()
main = do
  (env, _envs) <- loadDefaultEnvOrThrow defaultEnvFilePaths defaultDebugHooks

  result <- generateText env emptyConversation "what is the capital of france?"
  print result
  repl Nothing env
