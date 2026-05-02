module LLM.Chat (runChat) where

import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import LLM.Types
import System.Timeout (timeout)

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
