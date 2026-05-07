module LLM.TestKit where

import Data.Aeson (FromJSON, Value, decodeFileStrict)
import Data.IORef (newIORef)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import LLM (ChatEnv, ChatResponse (..), Conversation (..), LLMProviderAdapter (sendRequest), addUsage, emptyUsage, generateText, streamText)

data MockRequestResponse = MockRequestResponse
  { prompt :: Maybe Text,
    request :: Value,
    response :: Value
  }
  deriving (Generic)

instance FromJSON MockRequestResponse

type MockConversation = [MockRequestResponse]

-- | Reads a JSON file and returns the Value.
-- Returns Nothing if the file doesn't exist or contains invalid JSON.
readMockRequestResponse :: FilePath -> IO (Maybe MockConversation)
readMockRequestResponse = decodeFileStrict

loadRecordedConversation filePath = do
  s <- readMockRequestResponse filePath
  case s of
    Nothing -> error "can't read conversation"
    Just rrs ->
      let pairs = map (\rr -> (request rr, response rr)) rrs
          rrMap = M.fromList pairs
          prompts = rrs >>= \rsp -> case prompt rsp of Nothing -> []; Just p -> [p]
       in pure (rrMap, prompts)

mockProvider :: M.Map Value Value -> LLMProviderAdapter -> LLMProviderAdapter
mockProvider mp adapter =
  adapter
    { sendRequest = \val ->
        case M.lookup val mp of
          Nothing ->
            let q = fst (head (M.toList mp)) in error (show val <> "\n" <> show q)
          -- error ("vaulue not found" <> show val <> "\n" <> show (M.toList mp))
          -- error "value not found"
          Just r -> pure (200, r)
    }

streamChatLoopMain :: ChatEnv -> [Text] -> IO ()
streamChatLoopMain env prompts = do
  putStrLn "\n=== Ollama (with Claude  and Gemini fallbacks) ==="
  _ <- streamChatLoop env prompts
  pure ()

-- | Interactive streaming loop — runs a list of prompts, printing
-- streamed deltas and usage stats as it goes.
streamChatLoop :: ChatEnv -> [Text] -> IO Conversation
streamChatLoop env = aux emptyUsage (Conversation [])
  where
    aux totalUsage conv [] = do
      return conv
    aux totalUsage conv (prompt : rest) = do
      firstChunkRef <- newIORef True
      result <- streamText env conv prompt $ const $ pure ()
      case result of
        Left (err, _, _) -> do
          pure conv
        Right (_, conv', usage) -> do
          aux (addUsage totalUsage usage) conv' rest
