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
    Usage (..),
    PricingInfo (..),
    StreamEvent (..),
    RetryConfig (..),
    LogLevel (..),
    Logger,
    defaultChatConfig,
    defaultRequest,
    defaultRetryConfig,
    noRetry,
    noLogger,
    stderrLogger,
    hasToolCalls,
    getToolCalls,
    executeTool,
    executeTools,
    user,
    assistant,
    toolResult,
    emptyUsage,
    addUsage,
    estimateCost,
    isRetryable,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.IO (stderr)

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
  { trCallId :: Text, -- unique call id (matches tcId)
    trName :: Text, -- function name (matches tcName)
    trContent :: Text
  }
  deriving (Show, Eq)

-- | Smart constructor for tool results
toolResult :: ToolCall -> Text -> ToolResult
toolResult tc = ToolResult (tcId tc) (tcName tc)

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
    cfgMaxToolRounds :: Int, -- safety limit to prevent infinite loops
    cfgRequestTimeout :: Maybe Int, -- per-request timeout in microseconds
    cfgRetry :: RetryConfig,
    cfgLogger :: Logger
  }

instance Show ChatConfig where
  show cfg =
    "ChatConfig {cfgModel = "
      <> show (cfgModel cfg)
      <> ", cfgSystem = "
      <> show (cfgSystem cfg)
      <> ", cfgMaxTokens = "
      <> show (cfgMaxTokens cfg)
      <> ", cfgTemperature = "
      <> show (cfgTemperature cfg)
      <> ", cfgMaxToolRounds = "
      <> show (cfgMaxToolRounds cfg)
      <> ", cfgRequestTimeout = "
      <> show (cfgRequestTimeout cfg)
      <> ", cfgRetry = "
      <> show (cfgRetry cfg)
      <> ", cfgLogger = <function>}"

-- | Sensible defaults for chat config
defaultChatConfig :: Text -> ChatConfig
defaultChatConfig model =
  ChatConfig
    { cfgModel = model,
      cfgSystem = Nothing,
      cfgMaxTokens = 1024,
      cfgTemperature = Nothing,
      cfgMaxToolRounds = 10,
      cfgRequestTimeout = Nothing,
      cfgRetry = defaultRetryConfig,
      cfgLogger = noLogger
    }

-- | Retry configuration with exponential backoff
data RetryConfig = RetryConfig
  { retryMaxAttempts :: Int, -- max retries (0 = no retry)
    retryBaseDelay :: Int -- base delay in microseconds
  }
  deriving (Show)

defaultRetryConfig :: RetryConfig
defaultRetryConfig =
  RetryConfig
    { retryMaxAttempts = 3,
      retryBaseDelay = 1_000_000 -- 1 second
    }

noRetry :: RetryConfig
noRetry = RetryConfig {retryMaxAttempts = 0, retryBaseDelay = 0}

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

-- | Token usage from a single API call
data Usage = Usage
  { usageInputTokens :: Int,
    usageOutputTokens :: Int
  }
  deriving (Show, Eq)

emptyUsage :: Usage
emptyUsage = Usage 0 0

addUsage :: Usage -> Usage -> Usage
addUsage a b =
  Usage
    { usageInputTokens = usageInputTokens a + usageInputTokens b,
      usageOutputTokens = usageOutputTokens a + usageOutputTokens b
    }

-- | Pricing in dollars per million tokens
data PricingInfo = PricingInfo
  { pricePerMillionInput :: Double,
    pricePerMillionOutput :: Double
  }
  deriving (Show)

estimateCost :: PricingInfo -> Usage -> Double
estimateCost p u =
  fromIntegral (usageInputTokens u) * pricePerMillionInput p / 1_000_000
    + fromIntegral (usageOutputTokens u) * pricePerMillionOutput p / 1_000_000

data ChatResponse = ChatResponse
  { respText :: Text,
    respContent :: [ContentBlock],
    respUsage :: Maybe Usage
  }
  deriving (Show)

-- | Events emitted during streaming
data StreamEvent
  = StreamDelta Text -- incremental text chunk
  | StreamToolCall ToolCall -- complete tool call
  deriving (Show)

data LLMError
  = HttpError Int Text -- status code + raw body
  | NetworkError Text -- connection / DNS / TLS failure
  | TimeoutError -- request timed out
  | ParseError Text -- JSON we couldn't make sense of
  | EmptyResponse -- valid JSON, but no content in it
  | ToolLoopExceeded Int -- hit the max tool rounds limit
  deriving (Show)

-- | Whether an error is worth retrying
isRetryable :: LLMError -> Bool
isRetryable (HttpError status _) = status `elem` [429, 503, 529]
isRetryable (NetworkError _) = True
isRetryable _ = False

-- | Log verbosity levels, ordered from most to least verbose
data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord)

-- | A logger callback. The library calls it; the consumer decides what to do.
type Logger = LogLevel -> Text -> IO ()

-- | No-op logger (default)
noLogger :: Logger
noLogger _ _ = pure ()

-- | Simple stderr logger that filters by minimum level
stderrLogger :: LogLevel -> Logger
stderrLogger minLevel level msg
  | level >= minLevel = TIO.hPutStrLn stderr $ "[" <> T.pack (show level) <> "] " <> msg
  | otherwise = pure ()

type LLMResult = Either LLMError ChatResponse

data LLMClient = LLMClient
  { clientChat :: ChatRequest -> IO LLMResult,
    clientChatStream :: Maybe (ChatRequest -> (StreamEvent -> IO ()) -> IO LLMResult)
  }