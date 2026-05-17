module Adapters.Repl (repl, replMain) where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM (streamTextWithWorkers)
import LLM.Core.Logger
  ( LogLevel (Debug),
    noHooks,
    withJsonDump,
    withStderrLogger,
  )
import LLM.Core.Types (Conversation (..), StreamEvent (..))
import LLM.Core.Usage
  ( Usage (usageInputTokens, usageOutputTokens, usageTotalCost),
    addUsage,
    emptyUsage,
  )
import LLM.Generate.Types
  ( ChatEnv (..),
    WorkerMap,
  )
import LLM.Load.LoadEnvs (defaultEnvFilePaths, loadEnvOrThrow)
import LLM.Load.Types (LoadedEnvs (workerMap))
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, isEOF, stdout)
import Text.Printf (printf)

replMain :: (MonadUnliftIO m, MonadCatch m) => m ()
replMain = do
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
  (env, envs) <- loadEnvOrThrow defaultEnvFilePaths "default" hooks
  repl (workerMap envs) env

repl :: (MonadUnliftIO m, MonadCatch m) => Maybe (WorkerMap m) -> ChatEnv m -> m ()
repl mbWorkerMap env = do
  liftIO $ hSetBuffering stdout NoBuffering
  liftIO $ putStrLn "Type a message (or /quit to exit, /clear to reset conversation)."
  loop mbWorkerMap env emptyUsage (Conversation [])

loop :: (MonadUnliftIO m, MonadCatch m) => Maybe (WorkerMap m) -> ChatEnv m -> Usage -> Conversation -> m ()
loop mbWorkerMap env totalUsage conv = do
  eof <- liftIO $ do
    TIO.putStr "> "
    hFlush stdout
    isEOF
  if eof
    then liftIO $ printSummary totalUsage
    else do
      input <- liftIO $ T.strip <$> TIO.getLine
      case parseCommand input of
        Quit -> liftIO $ printSummary totalUsage
        Clear -> do
          liftIO $ putStrLn "(conversation cleared)"
          loop mbWorkerMap env emptyUsage (Conversation [])
        Chat "" -> loop mbWorkerMap env totalUsage conv
        Chat msg -> do
          result <- streamTextWithWorkers mbWorkerMap env conv msg $ \case
            StreamDelta txt -> TIO.putStr txt
            StreamToolCall tc -> TIO.putStrLn $ "  [tool call: " <> T.pack (show tc) <> "]"
          case result of
            Left (err, _, _) -> do
              liftIO $ putStrLn $ "\nError: " <> show err
              loop mbWorkerMap env totalUsage conv
            Right (_, conv', usage) -> do
              liftIO $ putStrLn ""
              let totalUsage' = addUsage totalUsage usage
              liftIO $
                printf
                  "  (%d turns, %d in + %d out tokens, $%.4f)\n"
                  (length $ unConversation conv')
                  (usageInputTokens usage)
                  (usageOutputTokens usage)
                  (usageTotalCost usage)
              loop mbWorkerMap env totalUsage' conv'

data Command = Quit | Clear | Chat Text

parseCommand :: Text -> Command
parseCommand t
  | stripped == "/quit" = Quit
  | stripped == "/clear" = Clear
  | otherwise = Chat t
  where
    stripped = T.strip t

printSummary :: Usage -> IO ()
printSummary u = do
  printf
    "\nSession total: %d input + %d output tokens, $%.4f\n"
    (usageInputTokens u)
    (usageOutputTokens u)
    (usageTotalCost u)
  exitSuccess
