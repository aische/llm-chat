module LLM.TestKit where

import Data.Aeson (FromJSON, Value, decodeFileStrict)
import Data.Aeson.Types (parseMaybe)
import Data.IORef (newIORef)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import LLM (ChatEnv, ChatResponse (..), Conversation (..), LLMProviderAdapter (..), addUsage, emptyUsage, generateText, parseChatResponse, streamText)

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
    { sendRequest = \val -> do
        case M.lookup val mp of
          Nothing ->
            -- error (show val <> "\n" <> show (fst $ head $ M.toList mp))
            error ("value not found for:" <> show val)
          Just r -> pure (200, r),
      sendStreamRequest = \body callback -> do
        case M.lookup body mp of
          Nothing ->
            -- error (show body <> "\n" <> show (fst $ head $ M.toList mp))
            error ("value not found for:" <> show body)
          Just r -> case parseMaybe parseChatResponse r of
            Nothing -> error "can't parse recorded ChatResponse json"
            Just chatResponse -> pure $ Right chatResponse
    }

streamChatLoopMain :: Bool -> ChatEnv -> [Text] -> IO ()
streamChatLoopMain stream env prompts = do
  putStrLn "\n=== Ollama (with Claude  and Gemini fallbacks) ==="
  _ <- streamChatLoop stream env prompts
  pure ()

streamChatLoop :: Bool -> ChatEnv -> [Text] -> IO Conversation
streamChatLoop stream env = aux emptyUsage (Conversation [])
  where
    aux totalUsage conv [] = do
      return conv
    aux totalUsage conv (prompt : rest) = do
      firstChunkRef <- newIORef True
      result <- if stream then streamText env conv prompt $ const (pure ()) else generateText env conv prompt
      case result of
        Left (err, _, _) -> do
          pure conv
        Right (_, conv', usage) -> do
          aux (addUsage totalUsage usage) conv' rest
