module LLM.Core.Utils
  ( withConversation,
    hasToolCalls,
    getToolCalls,
    executeTool,
    executeTools,
    executeToolsWithAbort,
    toolResult,
    isRetryable,
    withRetry,
    withTimeout,
    streamResponseJson,
  )
where

import Control.Exception (SomeException (SomeException), try)
import Control.Retry (RetryPolicyM, RetryStatus (rsIterNumber), retrying)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Abort (AbortSignal, isAborted)
import LLM.Core.Logger (LogLevel (Warn), Logger)
import LLM.Core.Types
  ( ChatResponse (..),
    ContentBlock (..),
    Conversation (..),
    LLMError (..),
    LLMResult (..),
    Tool (..),
    ToolCall (..),
    ToolContext (..),
    ToolDef (..),
    ToolResult (..),
    Turn,
  )
import LLM.Core.Usage (Usage (..))
import System.Directory.Internal.Prelude (fromMaybe, timeout)

withConversation :: Conversation -> ([Turn] -> [Turn]) -> Conversation
withConversation (Conversation turns) f = Conversation (f turns)

-- | Smart constructor for tool results
toolResult :: ToolCall -> Text -> ToolResult
toolResult tc = ToolResult (tcId tc) (tcName tc)

-- | Execute a single tool call by looking it up in the tool list
executeTool :: ToolContext -> [Tool] -> ToolCall -> IO ToolResult
executeTool ctx tools tc = case lookup (tcName tc) toolMap of
  Nothing -> pure $ toolResult tc ("Unknown tool: " <> tcName tc)
  Just exec -> do
    result <- try (exec ctx (tcArguments tc))
    case result of
      Right text -> pure $ toolResult tc text
      Left (e :: SomeException) ->
        pure $ toolResult tc ("Tool error: " <> T.pack (show e))
  where
    toolMap = [(toolName (toolDef t), toolExecute t) | t <- tools]

-- | Execute all tool calls from a response
executeTools :: ToolContext -> [Tool] -> [ToolCall] -> IO [ToolResult]
executeTools ctx tools = mapM (executeTool ctx tools)

-- | Execute tool calls one at a time, checking the abort signal between each.
-- Returns @Left Aborted@ if the signal fires before all calls finish.
executeToolsWithAbort :: Maybe AbortSignal -> ToolContext -> [Tool] -> [ToolCall] -> IO (Either LLMError [ToolResult])
executeToolsWithAbort Nothing ctx tools tcs = Right <$> executeTools ctx tools tcs
executeToolsWithAbort (Just sig) ctx tools tcs = go [] tcs
  where
    go acc [] = pure (Right (reverse acc))
    go acc (tc : rest) = do
      aborted <- isAborted sig
      if aborted
        then pure (Left Aborted)
        else do
          r <- executeTool ctx tools tc
          go (r : acc) rest

-- | Check whether a response contains tool calls
hasToolCalls :: ChatResponse -> Bool
hasToolCalls = not . null . getToolCalls

-- | Extract tool calls from a response
getToolCalls :: ChatResponse -> [ToolCall]
getToolCalls = concatMap go . respContent
  where
    go (ToolCallBlock tc) = [tc]
    go _ = []

-- | Whether an error is worth retrying
isRetryable :: LLMError -> Bool
isRetryable (HttpError status _) = status `elem` [429, 503, 529]
isRetryable (NetworkError _) = True
isRetryable _ = False

-- | Wrap an action with a timeout (ms). Returns 'TimeoutError' on expiry.
withTimeout :: Maybe Int -> IO LLMResult -> IO LLMResult
withTimeout Nothing action = action
withTimeout (Just us) action = do
  result <- timeout (us * 1000) action
  pure $ fromMaybe (ResError TimeoutError) result

-- | Retry an action using the retry package's policy (exponential backoff + jitter).
-- The policy controls max attempts, delays, and jitter.
withRetry :: RetryPolicyM IO -> Logger -> IO LLMResult -> IO LLMResult
withRetry policy log action =
  retrying
    policy
    ( \status result -> case result of
        ResError err | isRetryable err -> do
          log Warn $
            "Retryable error (attempt "
              <> T.pack (show (rsIterNumber status + 1))
              <> "): "
              <> T.pack (show err)
          pure True
        _ -> pure False
    )
    (const action)

-- | Build a synthetic JSON summary from a streamed ChatResponse,
-- used by providers to fire 'onResponse' after streaming completes.
streamResponseJson :: ChatResponse -> Value
streamResponseJson r =
  object
    [ "text" .= respText r,
      "content" .= map blockToJson (respContent r),
      "usage" .= fmap usageToJson (respUsage r)
    ]
  where
    blockToJson (TextBlock t) = object ["type" .= ("text" :: Text), "text" .= t]
    blockToJson (ToolCallBlock tc) =
      object
        [ "type" .= ("tool_call" :: Text),
          "id" .= tcId tc,
          "name" .= tcName tc,
          "arguments" .= tcArguments tc
        ]
    usageToJson u =
      object
        [ "input_tokens" .= usageInputTokens u,
          "output_tokens" .= usageOutputTokens u
        ]
