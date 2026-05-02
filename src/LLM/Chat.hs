{-# LANGUAGE LambdaCase #-}

module LLM.Chat (runChat, runChatLoop, streamChat, streamChatLoop) where

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Types
import System.Timeout (timeout)
import Text.Printf (printf)

-- | Run a chat with automatic tool-call handling.
--
-- Takes a client, config, tools, previous conversation, and a new user message.
-- Returns the final assistant text, the full updated conversation, and
-- accumulated token usage across all rounds, or an error.
--
-- Each API call is wrapped with the configured retry and timeout policies.
-- The tool loop runs until the model stops requesting tools or
-- 'cfgMaxToolRounds' is reached.
runChat ::
  LLMClient ->
  ChatConfig ->
  [Tool] ->
  Conversation ->
  Text ->
  IO (Either LLMError (Text, Conversation, Usage))
runChat client cfg tools conv msg = do
  let conv' = conv ++ [UserTurn msg]
  loop 0 emptyUsage conv'
  where
    loop :: Int -> Usage -> Conversation -> IO (Either LLMError (Text, Conversation, Usage))
    loop rounds acc conv'
      | rounds >= cfgMaxToolRounds cfg =
          pure $ Left (ToolLoopExceeded rounds)
      | otherwise = do
          let request =
                ChatRequest
                  { reqModel = cfgModel cfg,
                    reqConversation = conv',
                    reqSystem = cfgSystem cfg,
                    reqMaxTokens = cfgMaxTokens cfg,
                    reqTemperature = cfgTemperature cfg,
                    reqTools = map toolDef tools
                  }
          result <-
            withTimeout (cfgRequestTimeout cfg) $
              withRetry (cfgRetry cfg) $
                clientChat client request
          case result of
            Left err -> pure $ Left err
            Right resp ->
              let acc' = addUsage acc (fromMaybe emptyUsage (respUsage resp))
               in if hasToolCalls resp
                    then do
                      let calls = getToolCalls resp
                      results <- executeTools tools calls
                      let conv'' =
                            conv'
                              ++ [AssistantTurn (respText resp) calls]
                              ++ [ToolTurn results]
                      loop (rounds + 1) acc' conv''
                    else do
                      let finalConv =
                            conv'
                              ++ [AssistantTurn (respText resp) []]
                      pure $ Right (respText resp, finalConv, acc')

-- | Wrap an action with a timeout. Returns 'TimeoutError' on expiry.
withTimeout :: Maybe Int -> IO LLMResult -> IO LLMResult
withTimeout Nothing action = action
withTimeout (Just us) action = do
  result <- timeout us action
  pure $ fromMaybe (Left TimeoutError) result

-- | Like 'runChat', but streams text deltas via a callback as they arrive.
--
-- Falls back to 'runChat' if the client does not support streaming.
streamChat ::
  LLMClient ->
  ChatConfig ->
  [Tool] ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  IO (Either LLMError (Text, Conversation, Usage))
streamChat client cfg tools conv msg callback =
  case clientChatStream client of
    Nothing -> runChat client cfg tools conv msg
    Just stream -> do
      let conv' = conv ++ [UserTurn msg]
      sLoop 0 emptyUsage conv'
      where
        sLoop :: Int -> Usage -> Conversation -> IO (Either LLMError (Text, Conversation, Usage))
        sLoop rounds acc conv'
          | rounds >= cfgMaxToolRounds cfg =
              pure $ Left (ToolLoopExceeded rounds)
          | otherwise = do
              let request =
                    ChatRequest
                      { reqModel = cfgModel cfg,
                        reqConversation = conv',
                        reqSystem = cfgSystem cfg,
                        reqMaxTokens = cfgMaxTokens cfg,
                        reqTemperature = cfgTemperature cfg,
                        reqTools = map toolDef tools
                      }
              result <-
                withTimeout (cfgRequestTimeout cfg) $
                  withRetry (cfgRetry cfg) $
                    stream request callback
              case result of
                Left err -> pure $ Left err
                Right resp ->
                  let acc' = addUsage acc (fromMaybe emptyUsage (respUsage resp))
                   in if hasToolCalls resp
                        then do
                          let calls = getToolCalls resp
                          results <- executeTools tools calls
                          let conv'' =
                                conv'
                                  ++ [AssistantTurn (respText resp) calls]
                                  ++ [ToolTurn results]
                          sLoop (rounds + 1) acc' conv''
                        else do
                          let finalConv =
                                conv'
                                  ++ [AssistantTurn (respText resp) []]
                          pure $ Right (respText resp, finalConv, acc')

-- | Retry an action with exponential backoff on retryable errors.
withRetry :: RetryConfig -> IO LLMResult -> IO LLMResult
withRetry cfg action = go 0
  where
    go attempt
      | attempt >= retryMaxAttempts cfg = action
      | otherwise = do
          result <- action
          case result of
            Left err
              | isRetryable err -> do
                  threadDelay (retryBaseDelay cfg * (2 ^ attempt))
                  go (attempt + 1)
            _ -> pure result

runChatLoop :: LLMClient -> ChatConfig -> [Tool] -> PricingInfo -> [T.Text] -> IO Conversation
runChatLoop client cfg tools pricing = aux emptyUsage []
  where
    aux totalUsage conv [] = do
      putStrLn $
        "\n  Total: "
          <> show (usageInputTokens totalUsage)
          <> " input + "
          <> show (usageOutputTokens totalUsage)
          <> " output tokens"
      printf "  Estimated cost: $%.6f\n" (estimateCost pricing totalUsage)
      return conv
    aux totalUsage conv (prompt : rest) = do
      putStrLn $ "> " <> T.unpack prompt
      result <- runChat client cfg tools conv prompt
      case result of
        Left err -> do
          putStrLn $ "Error: " <> show err
          pure conv
        Right (text, conv', usage) -> do
          TIO.putStrLn text
          putStrLn $
            "  ("
              <> show (length conv')
              <> " turns, "
              <> show (usageInputTokens usage)
              <> " in + "
              <> show (usageOutputTokens usage)
              <> " out tokens)"
          aux (addUsage totalUsage usage) conv' rest

streamChatLoop :: LLMClient -> ChatConfig -> [Tool] -> PricingInfo -> [T.Text] -> IO Conversation
streamChatLoop client cfg tools pricing = aux emptyUsage []
  where
    aux totalUsage conv [] = do
      putStrLn $
        "\n  Total: "
          <> show (usageInputTokens totalUsage)
          <> " input + "
          <> show (usageOutputTokens totalUsage)
          <> " output tokens"
      printf "  Estimated cost: $%.6f\n" (estimateCost pricing totalUsage)
      return conv
    aux totalUsage conv (prompt : rest) = do
      putStrLn $ "> " <> T.unpack prompt
      firstChunkRef <- newIORef True
      result <- streamChat client cfg tools conv prompt $ \case
        StreamDelta txt -> do
          isFirst <- readIORef firstChunkRef
          writeIORef firstChunkRef False
          TIO.putStr txt
        StreamToolCall _ -> pure ()
      case result of
        Left err -> do
          putStrLn $ "Error: " <> show err
          pure conv
        Right (text, conv', usage) -> do
          putStrLn ""
          putStrLn $
            "  ("
              <> show (length conv')
              <> " turns, "
              <> show (usageInputTokens usage)
              <> " in + "
              <> show (usageOutputTokens usage)
              <> " out tokens)"
          aux (addUsage totalUsage usage) conv' rest
