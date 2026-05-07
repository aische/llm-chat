{-# LANGUAGE LambdaCase #-}

module Adapters.StreamChatLoop (streamChatLoop, streamChatLoopMain) where

import AllModels (AllModels (..), getAllModels)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Core.Generate (streamText)
import LLM.Core.LLMProvider (ChatEnv)
import LLM.Core.Types
  ( Conversation (..),
    StreamEvent (StreamDelta, StreamToolCall),
  )
import LLM.Core.Usage
  ( Usage (usageInputTokens, usageOutputTokens, usageTotalCost),
    addUsage,
    emptyUsage,
  )
import Text.Printf (printf)

prompts :: [Text]
prompts =
  [ "how old is John? Use a tool to look up his age",
    "how's the weather in london?",
    "and in paris?",
    "write a poem about that city. 16 lines."
  ]

streamChatLoopMain :: ChatEnv -> IO ()
streamChatLoopMain env = do
  putStrLn "\n=== Ollama (with Claude  and Gemini fallbacks) ==="
  _ <- streamChatLoop env prompts
  pure ()

-- | Interactive streaming loop — runs a list of prompts, printing
-- streamed deltas and usage stats as it goes.
streamChatLoop :: ChatEnv -> [Text] -> IO Conversation
streamChatLoop env = aux emptyUsage (Conversation [])
  where
    aux totalUsage conv [] = do
      putStrLn $
        "\n  Total: "
          <> show (usageInputTokens totalUsage)
          <> " input + "
          <> show (usageOutputTokens totalUsage)
          <> " output tokens"
      -- Use the primary model's pricing for the summary
      printf "  Estimated cost: $%.4f\n" (usageTotalCost totalUsage)
      return conv
    aux totalUsage conv (prompt : rest) = do
      putStrLn $ "> " <> T.unpack prompt
      firstChunkRef <- newIORef True
      result <- streamText env conv prompt $ \case
        StreamDelta txt -> do
          _ <- readIORef firstChunkRef
          writeIORef firstChunkRef False
          TIO.putStr txt
        StreamToolCall _ -> pure ()
      case result of
        Left (err, _, _) -> do
          putStrLn $ "Error: " <> show err
          pure conv
        Right (_, conv', usage) -> do
          putStrLn ""
          putStrLn $
            "  ("
              <> show (length $ unConversation conv')
              <> " turns, "
              <> show (usageInputTokens usage)
              <> " in + "
              <> show (usageOutputTokens usage)
              <> " out tokens)"
          aux (addUsage totalUsage usage) conv' rest
