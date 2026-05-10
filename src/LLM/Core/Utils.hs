module LLM.Core.Utils
  ( withConversation,
    emptyConversation,
    hasToolCalls,
    getToolCalls,
    toTool,
    executeTool,
    executeTools,
    executeToolsWithAbort,
    toolResult,
    isRetryable,
    withRetry,
    withTimeout,
    streamResponseJson,
    printValue,
    parseChatResponse,
  )
where

import Autodocodec qualified as AC
import Autodocodec.Schema (jsonSchemaVia)
import Control.Exception (SomeException, try)
import Control.Retry (RetryPolicyM, RetryStatus (rsIterNumber), retrying)
import Data.Aeson (FromJSON, Value, encode, object, (.=))
import Data.Aeson qualified as AE
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Abort (AbortSignal, isAborted)
import LLM.Core.Logger (LogLevel (Warn), Logger)
import LLM.Core.Types
  ( ChatResponse (..),
    ContentBlock (..),
    Conversation (..),
    LLMError (..),
    LLMResult,
    Tool (..),
    ToolCall (..),
    ToolContext (..),
    ToolDef (..),
    ToolResult (..),
    Turn,
    TypedTool (TypedTool),
  )
import LLM.Core.Usage (Usage (..))
import System.Timeout (timeout)

withConversation :: Conversation -> ([Turn] -> [Turn]) -> Conversation
withConversation (Conversation turns) f = Conversation (f turns)

emptyConversation :: Conversation
emptyConversation = Conversation []

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
withTimeout :: Maybe Int -> IO (LLMResult a) -> IO (LLMResult a)
withTimeout Nothing action = action
withTimeout (Just us) action = do
  result <- timeout (us * 1000) action
  pure $ fromMaybe (Left TimeoutError) result

-- | Retry an action using the retry package's policy (exponential backoff + jitter).
-- The policy controls max attempts, delays, and jitter.
withRetry :: RetryPolicyM IO -> Logger -> IO (LLMResult a) -> IO (LLMResult a)
withRetry policy logIt action =
  retrying
    policy
    ( \status result -> case result of
        Left err | isRetryable err -> do
          logIt Warn $
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

parseChatResponse :: Value -> Parser ChatResponse
parseChatResponse = AE.withObject "ChatResponse" $ \v -> do
  text <- v AE..: "text"
  content <- v AE..: "content" >>= mapM parseContentBlock
  usage <- v AE..:? "usage" >>= mapM parseUsage
  pure $ ChatResponse text content usage
  where
    parseContentBlock = AE.withObject "ContentBlock" $ \o -> do
      t <- o AE..: "type"
      case (t :: Text) of
        "text" -> TextBlock <$> o AE..: "text"
        "tool_call" -> do
          tcId <- o AE..: "id"
          tcName <- o AE..: "name"
          tcArgs <- o AE..: "arguments"
          pure $ ToolCallBlock $ ToolCall tcId tcName tcArgs
        _ -> fail "Unknown content block type"

    parseUsage = AE.withObject "Usage" $ \o -> do
      input <- o AE..: "input_tokens"
      output <- o AE..: "output_tokens"
      pure $ Usage input output 0.0

printValue :: Value -> IO ()
printValue val = L8.putStrLn (encode val)

getSchema :: (AC.HasCodec t, FromJSON t) => TypedTool t -> AC.JSONCodec t
getSchema _ = AC.codec

toTool :: (AC.HasCodec t, FromJSON t) => TypedTool t -> Tool
toTool t@(TypedTool name descr exec) =
  Tool
    { toolDef =
        ToolDef
          { toolName = name,
            toolDescription = descr,
            toolParameters = AE.toJSON $ jsonSchemaVia $ getSchema t
          },
      toolExecute = \ctx argsvalue ->
        case AE.fromJSON argsvalue of
          AE.Error _e -> pure "Error: Parsing arguments failed" -- TODO: e not used
          AE.Success args -> exec ctx args
    }
