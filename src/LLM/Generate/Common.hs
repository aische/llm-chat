module LLM.Generate.Common
  ( windowOffset,
    findNthUserFromEnd,
    modelRetryPolicy,
    mkRequest,
    filterReadonlyTools,
    requestLogMessage,
    toolCallsLogMessage,
    toolResultsLogMessage,
    responseLogMessage,
    mkRequestWithWorkers,
    getFilteredToolsWithWorkers,
  )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Control.Retry (RetryPolicyM, fullJitterBackoff, limitRetries)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types
  ( ChatRequest (..),
    ChatResponse (..),
    Conversation (unConversation),
    Tool (..),
    ToolCall (..),
    ToolDef (..),
    ToolResult (..),
    Turn (UserTurn),
  )
import LLM.Core.Usage (Usage (usageInputTokens, usageOutputTokens))
import LLM.Core.Utils (withConversation)
import LLM.Generate.Types (ChatEnv (..), GenerateText, ModelConfig (..), WorkerMap)
import LLM.Generate.Utils (getToolsWithWorkers)

-- | Compute the index where the visible window starts.
-- The window includes the last @n@ user messages and all turns that follow
-- each of them (assistant replies, tool rounds, etc.).
-- Returns 0 (no windowing) when the window is 'Nothing' or the conversation
-- contains fewer than @n@ user messages.
windowOffset :: Maybe Int -> Conversation -> Int
windowOffset Nothing _ = 0
windowOffset (Just n) conv = findNthUserFromEnd n conv

-- | Find the index of the Nth 'UserTurn' from the end of a conversation.
-- Returns 0 if there are fewer than @n@ user messages.
findNthUserFromEnd :: Int -> Conversation -> Int
findNthUserFromEnd 0 _conv = 0
findNthUserFromEnd n conv = go (length (unConversation conv) - 1) n
  where
    go idx remaining
      | idx < 0 = 0
      | remaining <= 0 = idx + 1
      | otherwise = case unConversation conv !! idx of
          UserTurn _ -> go (idx - 1) (remaining - 1)
          _ -> go (idx - 1) remaining

modelRetryPolicy :: (MonadIO m) => ModelConfig -> RetryPolicyM m
modelRetryPolicy mc = limitRetries (mcRetryCount mc) <> fullJitterBackoff (mcJitterBackoff mc * 1000)

-- | Build a ChatRequest from the model config and a conversation.
-- When 'envContextWindow' is set, only the last N user messages (and their
-- associated replies) are sent to the model.
mkRequest :: (MonadUnliftIO m, MonadCatch m) => ChatEnv m -> ModelConfig -> Conversation -> Bool -> ChatRequest
mkRequest = mkRequestWithWorkers Nothing

mkRequestWithWorkers :: (MonadUnliftIO m, MonadCatch m) => Maybe (GenerateText m, WorkerMap m) -> ChatEnv m -> ModelConfig -> Conversation -> Bool -> ChatRequest
mkRequestWithWorkers mbGenWorkerMap env mc conv readonly =
  ChatRequest
    { reqModel = mcModel mc,
      reqConversation = withConversation conv (drop offset),
      reqSystem = envSystem env,
      reqMaxTokens = mcMaxTokens mc,
      reqTemperature = mcTemperature mc,
      -- reqTools = map toolDef (filterReadonlyTools readonly $ envTools env)
      reqTools = map toolDef (getFilteredToolsWithWorkers mbGenWorkerMap readonly env)
    }
  where
    offset = windowOffset (envContextWindow env) conv

getFilteredToolsWithWorkers :: (MonadUnliftIO m, MonadCatch m) => Maybe (GenerateText m, WorkerMap m) -> Bool -> ChatEnv m -> [Tool m]
getFilteredToolsWithWorkers mbGenWorkerMap readonly env =
  filterReadonlyTools readonly (getToolsWithWorkers mbGenWorkerMap env)

filterReadonlyTools :: Bool -> [Tool m] -> [Tool m]
filterReadonlyTools False tools = tools
filterReadonlyTools True tools = filter (toolReadonly . toolDef) tools

requestLogMessage :: ModelConfig -> Int -> ChatRequest -> Text
requestLogMessage mc rounds request =
  "API request: model="
    <> mcModel mc
    <> " round="
    <> T.pack (show rounds)
    <> " turns="
    <> T.pack (show (length (unConversation (reqConversation request))))

toolCallsLogMessage :: [ToolCall] -> Text
toolCallsLogMessage calls = "Tool calls: " <> T.intercalate ", " (map tcName calls)

toolResultsLogMessage :: [ToolResult] -> Text
toolResultsLogMessage results =
  "Tool results: "
    <> T.intercalate
      ", "
      [trName r <> "=" <> T.take 100 (trContent r) | r <- results]

responseLogMessage :: ChatResponse -> Text
responseLogMessage resp =
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
