module Adapters.Repl (repl, replMain) where

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

replMain :: IO ()
replMain = do
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
  (env, envs) <- loadEnvOrThrow defaultEnvFilePaths "default" hooks
  repl (workerMap envs) env

repl :: Maybe WorkerMap -> ChatEnv -> IO ()
repl mbWorkerMap env = do
  hSetBuffering stdout NoBuffering
  putStrLn "Type a message (or /quit to exit, /clear to reset conversation)."
  loop mbWorkerMap env emptyUsage (Conversation [])

loop :: Maybe WorkerMap -> ChatEnv -> Usage -> Conversation -> IO ()
loop mbWorkerMap env totalUsage conv = do
  TIO.putStr "> "
  hFlush stdout
  eof <- isEOF
  if eof
    then printSummary totalUsage
    else do
      input <- T.strip <$> TIO.getLine
      case parseCommand input of
        Quit -> printSummary totalUsage
        Clear -> do
          putStrLn "(conversation cleared)"
          loop mbWorkerMap env emptyUsage (Conversation [])
        Chat "" -> loop mbWorkerMap env totalUsage conv
        Chat msg -> do
          result <- streamTextWithWorkers mbWorkerMap env conv msg $ \case
            StreamDelta txt -> TIO.putStr txt
            StreamToolCall tc -> TIO.putStrLn $ "  [tool call: " <> T.pack (show tc) <> "]"
          case result of
            Left (err, _, _) -> do
              putStrLn $ "\nError: " <> show err
              loop mbWorkerMap env totalUsage conv
            Right (_, conv', usage) -> do
              putStrLn ""
              let totalUsage' = addUsage totalUsage usage
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
