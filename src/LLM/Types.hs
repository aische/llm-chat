module LLM.Types
  ( Role (..),
    Message (..),
    ContentBlock (..),
    ChatRequest (..),
    ChatResponse (..),
    LLMError (..),
    LLMResult,
    LLMClient (..),
    ToolDef (..),
    ToolCall (..),
    ToolResult (..),
    defaultRequest,
    hasToolCalls,
    user,
    assistant,
    toolResult,
  )
where

import Data.Aeson (Value)
import Data.Text (Text)

data Role = User | Assistant
  deriving (Show, Eq)

data Message = Message
  { msgRole :: Role,
    msgContent :: Text
  }
  deriving (Show, Eq)

-- Smart constructors — these are what you'll use everywhere
user :: Text -> Message
user = Message User

assistant :: Text -> Message
assistant = Message Assistant

-- | A tool definition sent to the model
data ToolDef = ToolDef
  { toolName :: Text,
    toolDescription :: Text,
    toolParameters :: Value -- JSON Schema object
  }
  deriving (Show)

-- | A tool invocation returned by the model
data ToolCall = ToolCall
  { tcId :: Text, -- provider-specific call id
    tcName :: Text,
    tcArguments :: Value
  }
  deriving (Show, Eq)

-- | The result of executing a tool, sent back to the model
data ToolResult = ToolResult
  { trCallId :: Text,
    trContent :: Text
  }
  deriving (Show, Eq)

-- | Smart constructor for tool results
toolResult :: ToolCall -> Text -> ToolResult
toolResult tc = ToolResult (tcId tc)

-- | A content block in a response — either text or a tool call
data ContentBlock
  = TextBlock Text
  | ToolCallBlock ToolCall
  deriving (Show, Eq)

data ChatRequest = ChatRequest
  { reqModel :: Text,
    reqMessages :: [Message],
    reqSystem :: Maybe Text,
    reqMaxTokens :: Int,
    reqTemperature :: Maybe Double,
    reqTools :: [ToolDef],
    reqPendingToolCalls :: [ToolCall], -- assistant's tool calls from prev turn
    reqToolResults :: [ToolResult]
  }
  deriving (Show)

defaultRequest :: Text -> [Message] -> ChatRequest
defaultRequest model msgs =
  ChatRequest
    { reqModel = model,
      reqMessages = msgs,
      reqSystem = Nothing,
      reqMaxTokens = 1024,
      reqTemperature = Nothing,
      reqTools = [],
      reqPendingToolCalls = [],
      reqToolResults = []
    }

-- | Check whether a response contains tool calls
hasToolCalls :: ChatResponse -> Bool
hasToolCalls = any isToolCall . respContent
  where
    isToolCall (ToolCallBlock _) = True
    isToolCall _ = False

data ChatResponse = ChatResponse
  { respText :: Text,
    respContent :: [ContentBlock]
  }
  deriving (Show)

data LLMError
  = HttpError Int Text -- status code + raw body
  | ParseError Text -- JSON we couldn't make sense of
  | EmptyResponse -- valid JSON, but no content in it
  deriving (Show)

type LLMResult = Either LLMError ChatResponse

newtype LLMClient = LLMClient
  { clientChat :: ChatRequest -> IO LLMResult
  }