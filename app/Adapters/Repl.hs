{-# LANGUAGE LambdaCase #-}

module Adapters.Repl (repl) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Core.Chat (streamChat)
import LLM.Core.LLMProvider (ChatEnv)
import LLM.Core.Types (Conversation (..), StreamEvent (..))
import LLM.Core.Usage
  ( Usage (usageInputTokens, usageOutputTokens, usageTotalCost),
    addUsage,
    emptyUsage,
  )
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, isEOF, stdin, stdout)
import Text.Printf (printf)

repl :: ChatEnv -> IO ()
repl env = do
  hSetBuffering stdout NoBuffering
  putStrLn "Type a message (or /quit to exit, /clear to reset conversation)."
  loop env emptyUsage (Conversation [])

loop :: ChatEnv -> Usage -> Conversation -> IO ()
loop env totalUsage conv = do
  TIO.putStr "> "
  hFlush stdout
  eof <- isEOF
  if eof
    then printSummary totalUsage
    else do
      input <- TIO.getLine
      case parseCommand input of
        Quit -> printSummary totalUsage
        Clear -> do
          putStrLn "(conversation cleared)"
          loop env emptyUsage (Conversation [])
        Chat msg -> do
          result <- streamChat env conv msg $ \case
            StreamDelta txt -> TIO.putStr txt
            StreamToolCall tc -> TIO.putStrLn $ "  [tool call: " <> T.pack (show tc) <> "]"
          case result of
            Left (err, _, _) -> do
              putStrLn $ "\nError: " <> show err
              loop env totalUsage conv
            Right (_, conv', usage) -> do
              putStrLn ""
              let totalUsage' = addUsage totalUsage usage
              printf
                "  (%d turns, %d in + %d out tokens, $%.4f)\n"
                (length $ unConversation conv')
                (usageInputTokens usage)
                (usageOutputTokens usage)
                (usageTotalCost usage)
              loop env totalUsage' conv'

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
