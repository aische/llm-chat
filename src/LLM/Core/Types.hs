module LLM.Core.Types
  ( Turn (..),
    Conversation,
    ToolContext (..),
    ContentBlock (..),
    ChatRequest (..),
    ChatResponse (..),
    LLMError (..),
    LLMResult,
    Tool (..),
    ToolDef (..),
    ToolCall (..),
    ToolResult (..),
    StreamEvent (..),
    hasToolCalls,
    getToolCalls,
    executeTool,
    executeTools,
    toolResult,
    isRetryable,
    streamResponseJson,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Usage (Usage, usageInputTokens, usageOutputTokens)

-- | A single turn in a conversation
data Turn
  = UserTurn Text
  | AssistantTurn Text [ToolCall] -- text (possibly empty) + any tool calls
  | ToolTurn [ToolResult]
  deriving (Show, Eq)

-- | A full conversation history
type Conversation = [Turn]

-- | Context passed to tool implementations during execution.
-- Provides read access to the full (unwindowed) conversation and
-- accumulated token usage so far.
data ToolContext = ToolContext
  { -- | Full conversation history (not windowed)
    tcConversation :: Conversation,
    -- | Accumulated token usage so far
    tcUsage :: Usage,
    -- | Index into 'tcConversation' where the visible window starts.
    -- Everything before this index is hidden from the model.
    -- A @get_history@ tool can use this to serve paginated history.
    tcWindowOffset :: Int
  }
  deriving (Show, Eq)

-- | A tool definition sent to the model
data ToolDef = ToolDef
  { toolName :: Text,
    toolDescription :: Text,
    toolParameters :: Value -- JSON Schema object
  }
  deriving (Show, Eq)

-- | A tool invocation returned by the model
data ToolCall = ToolCall
  { tcId :: Text, -- provider-specific call id
    tcName :: Text,
    tcArguments :: Value
  }
  deriving (Show, Eq)

-- | The result of executing a tool, sent back to the model
data ToolResult = ToolResult
  { trCallId :: Text, -- unique call id (matches tcId)
    trName :: Text, -- function name (matches tcName)
    trContent :: Text
  }
  deriving (Show, Eq)

-- | Smart constructor for tool results
toolResult :: ToolCall -> Text -> ToolResult
toolResult tc = ToolResult (tcId tc) (tcName tc)

-- | A tool: its definition (sent to the model) paired with its implementation.
-- 'toolExecute' receives a 'ToolContext' (full conversation + usage) and
-- the JSON arguments from the model.
data Tool = Tool
  { toolDef :: ToolDef,
    toolExecute :: ToolContext -> Value -> IO Text
  }

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

-- | A content block in a response — either text or a tool call
data ContentBlock
  = TextBlock Text
  | ToolCallBlock ToolCall
  deriving (Show, Eq)

data ChatRequest = ChatRequest
  { reqModel :: Text,
    reqConversation :: Conversation,
    reqSystem :: Maybe Text,
    reqMaxTokens :: Int,
    reqTemperature :: Maybe Double,
    reqTools :: [ToolDef]
  }
  deriving (Show, Eq)

-- | Check whether a response contains tool calls
hasToolCalls :: ChatResponse -> Bool
hasToolCalls = not . null . getToolCalls

-- | Extract tool calls from a response
getToolCalls :: ChatResponse -> [ToolCall]
getToolCalls = concatMap go . respContent
  where
    go (ToolCallBlock tc) = [tc]
    go _ = []

data ChatResponse = ChatResponse
  { respText :: Text,
    respContent :: [ContentBlock],
    respUsage :: Maybe Usage
  }
  deriving (Show, Eq)

-- | Events emitted during streaming
data StreamEvent
  = StreamDelta Text -- incremental text chunk
  | StreamToolCall ToolCall -- complete tool call
  deriving (Show, Eq)

data LLMError
  = HttpError Int Text -- status code + raw body
  | NetworkError Text -- connection / DNS / TLS failure
  | TimeoutError -- request timed out
  | ParseError Text -- JSON we couldn't make sense of
  | EmptyResponse -- valid JSON, but no content in it
  | ToolLoopExceeded Int -- hit the max tool rounds limit
  deriving (Show, Eq)

-- | Whether an error is worth retrying
isRetryable :: LLMError -> Bool
isRetryable (HttpError status _) = status `elem` [429, 503, 529]
isRetryable (NetworkError _) = True
isRetryable _ = False

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

type LLMResult = Either LLMError ChatResponse