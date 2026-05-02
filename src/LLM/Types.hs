module LLM.Types
  ( Role (..),
    Message (..),
    Turn (..),
    Conversation,
    ContentBlock (..),
    ChatConfig (..),
    ChatRequest (..),
    ChatResponse (..),
    LLMError (..),
    LLMResult,
    LLMClient (..),
    Tool (..),
    ToolDef (..),
    ToolCall (..),
    ToolResult (..),
    defaultChatConfig,
    defaultRequest,
    hasToolCalls,
    getToolCalls,
    executeTool,
    executeTools,
    user,
    assistant,
    toolResult,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as T

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

-- | A single turn in a conversation
data Turn
  = UserTurn Text
  | AssistantTurn Text [ToolCall] -- text (possibly empty) + any tool calls
  | ToolTurn [ToolResult]
  deriving (Show, Eq)

-- | A full conversation history
type Conversation = [Turn]

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

-- | A tool: its definition (sent to the model) paired with its implementation
data Tool = Tool
  { toolDef :: ToolDef,
    toolExecute :: Value -> IO Text -- takes arguments JSON, returns result text
  }

-- | Execute a single tool call by looking it up in the tool list
executeTool :: [Tool] -> ToolCall -> IO ToolResult
executeTool tools tc = case lookup (tcName tc) toolMap of
  Nothing -> pure $ toolResult tc ("Unknown tool: " <> tcName tc)
  Just exec -> do
    result <- try (exec (tcArguments tc))
    case result of
      Right text -> pure $ toolResult tc text
      Left (e :: SomeException) ->
        pure $ toolResult tc ("Tool error: " <> T.pack (show e))
  where
    toolMap = [(toolName (toolDef t), toolExecute t) | t <- tools]

-- | Execute all tool calls from a response
executeTools :: [Tool] -> [ToolCall] -> IO [ToolResult]
executeTools tools = mapM (executeTool tools)

-- | A content block in a response — either text or a tool call
data ContentBlock
  = TextBlock Text
  | ToolCallBlock ToolCall
  deriving (Show, Eq)

-- | Configuration for a chat session
data ChatConfig = ChatConfig
  { cfgModel :: Text,
    cfgSystem :: Maybe Text,
    cfgMaxTokens :: Int,
    cfgTemperature :: Maybe Double,
    cfgMaxToolRounds :: Int -- safety limit to prevent infinite loops
  }
  deriving (Show)

-- | Sensible defaults for chat config
defaultChatConfig :: Text -> ChatConfig
defaultChatConfig model =
  ChatConfig
    { cfgModel = model,
      cfgSystem = Nothing,
      cfgMaxTokens = 1024,
      cfgTemperature = Nothing,
      cfgMaxToolRounds = 10
    }

data ChatRequest = ChatRequest
  { reqModel :: Text,
    reqConversation :: Conversation,
    reqSystem :: Maybe Text,
    reqMaxTokens :: Int,
    reqTemperature :: Maybe Double,
    reqTools :: [ToolDef]
  }
  deriving (Show)

defaultRequest :: Text -> [Message] -> ChatRequest
defaultRequest model msgs =
  ChatRequest
    { reqModel = model,
      reqConversation = map (\m -> case msgRole m of User -> UserTurn (msgContent m); Assistant -> AssistantTurn (msgContent m) []) msgs,
      reqSystem = Nothing,
      reqMaxTokens = 1024,
      reqTemperature = Nothing,
      reqTools = []
    }

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
    respContent :: [ContentBlock]
  }
  deriving (Show)

data LLMError
  = HttpError Int Text -- status code + raw body
  | ParseError Text -- JSON we couldn't make sense of
  | EmptyResponse -- valid JSON, but no content in it
  | ToolLoopExceeded Int -- hit the max tool rounds limit
  deriving (Show)

type LLMResult = Either LLMError ChatResponse

newtype LLMClient = LLMClient
  { clientChat :: ChatRequest -> IO LLMResult
  }