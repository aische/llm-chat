{-# LANGUAGE DeriveGeneric #-}

module LLM.Core.Types
  ( Turn (..),
    Conversation (..),
    ToolContext (..),
    ContentBlock (..),
    ChatRequest (..),
    GeneratedResult,
    ChatResponse (..),
    LLMError (..),
    LLMTextResult,
    LLMObjectResult,
    LLMResult,
    Tool (..),
    TypedTool (..),
    ToolDef (..),
    ToolCall (..),
    ToolResult (..),
    StreamEvent (..),
    Generatable,
  )
where

import Autodocodec (HasCodec)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import LLM.Core.Abort (AbortSignal)
import LLM.Core.Usage (Usage)

-- | A single turn in a conversation
data Turn
  = UserTurn Text
  | AssistantTurn Text [ToolCall] -- text (possibly empty) + any tool calls
  | ToolTurn [ToolResult]
  deriving (Show, Eq, Generic)

instance ToJSON Turn

instance FromJSON Turn

-- | A full conversation history
newtype Conversation = Conversation {unConversation :: [Turn]}
  deriving (Show, Eq, Generic, Semigroup)

instance ToJSON Conversation

instance FromJSON Conversation

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
    tcWindowOffset :: Int,
    -- | Optional abort signal; tools can check this to bail out early.
    tcAbortSignal :: Maybe AbortSignal
  }

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
  deriving (Show, Eq, Generic)

instance ToJSON ToolCall

instance FromJSON ToolCall

-- | The result of executing a tool, sent back to the model
data ToolResult = ToolResult
  { trCallId :: Text, -- unique call id (matches tcId)
    trName :: Text, -- function name (matches tcName)
    trContent :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolResult

instance FromJSON ToolResult

-- | A tool: its definition (sent to the model) paired with its implementation.
-- 'toolExecute' receives a 'ToolContext' (full conversation + usage) and
-- the JSON arguments from the model.
data Tool = Tool
  { toolDef :: ToolDef,
    toolExecute :: ToolContext -> Value -> IO Text
  }

data TypedTool a = TypedTool
  { ttoolName :: Text,
    ttoolDescription :: Text,
    ttoolExecute :: ToolContext -> a -> IO Text
  }

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
  | Aborted -- user cancelled the request
  deriving (Show, Eq, Generic)

instance FromJSON LLMError

instance ToJSON LLMError

-- | Result of an LLM operation: either an error, a chat response, or a generated object
type LLMTextResult = LLMResult ChatResponse

type LLMObjectResult = LLMResult Value

type LLMResult a = Either LLMError a

type GeneratedResult a = Either (LLMError, Conversation, Usage) a

class (HasCodec t, FromJSON t) => Generatable t
