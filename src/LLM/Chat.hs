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
  let log = cfgLogger cfg
      conv' = conv ++ [UserTurn msg]
  log Info $ "runChat: model=" <> cfgModel cfg <> " tools=" <> T.pack (show (length tools))
  loop log 0 emptyUsage conv'
  where
    loop :: Logger -> Int -> Usage -> Conversation -> IO (Either LLMError (Text, Conversation, Usage))
    loop log rounds acc conv'
      | rounds >= cfgMaxToolRounds cfg = do
          log Error $ "Tool loop exceeded: " <> T.pack (show rounds) <> " rounds"
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
          log Debug $
            "API request: round="
              <> T.pack (show rounds)
              <> " turns="
              <> T.pack (show (length (reqConversation request)))
          result <-
            withTimeout (cfgRequestTimeout cfg) $
              withRetry (cfgRetry cfg) log $
                clientChat client request
          case result of
            Left err -> do
              log Error $ "API error: " <> T.pack (show err)
              pure $ Left err
            Right resp ->
              let acc' = addUsage acc (fromMaybe emptyUsage (respUsage resp))
               in if hasToolCalls resp
                    then do
                      let calls = getToolCalls resp
                      log Info $ "Tool calls: " <> T.intercalate ", " (map tcName calls)
                      results <- executeTools tools calls
                      log Debug $
                        "Tool results: "
                          <> T.intercalate
                            ", "
                            [trName r <> "=" <> T.take 100 (trContent r) | r <- results]
                      let conv'' =
                            conv'
                              ++ [AssistantTurn (respText resp) calls]
                              ++ [ToolTurn results]
                      loop log (rounds + 1) acc' conv''
                    else do
                      log Info $
                        "Response: "
                          <> T.take 100 (respText resp)
                          <> maybe
                            ""
                            ( \u ->
                                " usage="
                                  <> T.pack (show (usageInputTokens u))
                                  <> "+"
                                  <> T.pack (show (usageOutputTokens u))
                            )
                            (respUsage resp)
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
      let log = cfgLogger cfg
          conv' = conv ++ [UserTurn msg]
      log Info $ "streamChat: model=" <> cfgModel cfg <> " tools=" <> T.pack (show (length tools))
      sLoop log 0 emptyUsage conv'
      where
        sLoop :: Logger -> Int -> Usage -> Conversation -> IO (Either LLMError (Text, Conversation, Usage))
        sLoop log rounds acc conv'
          | rounds >= cfgMaxToolRounds cfg = do
              log Error $ "Tool loop exceeded: " <> T.pack (show rounds) <> " rounds"
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
              log Debug $
                "Stream request: round="
                  <> T.pack (show rounds)
                  <> " turns="
                  <> T.pack (show (length (reqConversation request)))
              result <-
                withTimeout (cfgRequestTimeout cfg) $
                  withRetry (cfgRetry cfg) log $
                    stream request callback
              case result of
                Left err -> do
                  log Error $ "Stream error: " <> T.pack (show err)
                  pure $ Left err
                Right resp ->
                  let acc' = addUsage acc (fromMaybe emptyUsage (respUsage resp))
                   in if hasToolCalls resp
                        then do
                          let calls = getToolCalls resp
                          log Info $ "Tool calls: " <> T.intercalate ", " (map tcName calls)
                          results <- executeTools tools calls
                          log Debug $
                            "Tool results: "
                              <> T.intercalate
                                ", "
                                [trName r <> "=" <> T.take 100 (trContent r) | r <- results]
                          let conv'' =
                                conv'
                                  ++ [AssistantTurn (respText resp) calls]
                                  ++ [ToolTurn results]
                          sLoop log (rounds + 1) acc' conv''
                        else do
                          log Info $
                            "Response: "
                              <> T.take 100 (respText resp)
                              <> maybe
                                ""
                                ( \u ->
                                    " usage="
                                      <> T.pack (show (usageInputTokens u))
                                      <> "+"
                                      <> T.pack (show (usageOutputTokens u))
                                )
                                (respUsage resp)
                          let finalConv =
                                conv'
                                  ++ [AssistantTurn (respText resp) []]
                          pure $ Right (respText resp, finalConv, acc')

-- | Retry an action with exponential backoff on retryable errors.
withRetry :: RetryConfig -> Logger -> IO LLMResult -> IO LLMResult
withRetry cfg log action = go 0
  where
    go attempt
      | attempt >= retryMaxAttempts cfg = action
      | otherwise = do
          result <- action
          case result of
            Left err
              | isRetryable err -> do
                  let delayUs = retryBaseDelay cfg * (2 ^ attempt)
                  log Warn $
                    "Retryable error (attempt "
                      <> T.pack (show (attempt + 1))
                      <> "/"
                      <> T.pack (show (retryMaxAttempts cfg))
                      <> ", delay "
                      <> T.pack (show (delayUs `div` 1000))
                      <> "ms): "
                      <> T.pack (show err)
                  threadDelay delayUs
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
