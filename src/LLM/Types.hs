module LLM.Types
  ( Role (..),
    Message (..),
    ChatRequest (..),
    ChatResponse (..),
    LLMError (..),
    LLMResult,
    LLMClient (..),
    defaultRequest,
    user,
    assistant,
  )
where

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

data ChatRequest = ChatRequest
  { reqModel :: Text,
    reqMessages :: [Message],
    reqSystem :: Maybe Text,
    reqMaxTokens :: Int,
    reqTemperature :: Maybe Double
  }
  deriving (Show)

defaultRequest :: Text -> [Message] -> ChatRequest
defaultRequest model msgs =
  ChatRequest
    { reqModel = model,
      reqMessages = msgs,
      reqSystem = Nothing,
      reqMaxTokens = 1024,
      reqTemperature = Nothing
    }

newtype ChatResponse = ChatResponse
  { respText :: Text
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