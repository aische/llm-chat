module LLM.TestKit where

import Data.Aeson (FromJSON, Value, decodeFileStrict)
import Data.Aeson.Types (parseMaybe)
-- import Data.IORef (newIORef)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import LLM.Core.LLMProvider ( LLMProvider(..) )
import LLM.Core.Types ( Conversation(..) )
import LLM.Core.Usage ( addUsage, emptyUsage )
import LLM.Core.Utils ( parseChatResponse )
import LLM.Generate.Generate ( generateText, streamText )
import LLM.Generate.Types ( ChatEnv )
import LLM.Generate.Chat (generateTextSimple, streamTextSimple)

data MockRequestResponse = MockRequestResponse
  { prompt :: Maybe Text,
    request :: Value,
    response :: Value
  }
  deriving (Generic)
  deriving anyclass (FromJSON)

type MockConversation = [MockRequestResponse]

type MockConversationMap = M.Map Value Value

-- | Reads a JSON file and returns the Value.
-- Returns Nothing if the file doesn't exist or contains invalid JSON.
readMockRequestResponse :: FilePath -> IO (Maybe MockConversation)
readMockRequestResponse = decodeFileStrict

loadRecordedConversation :: FilePath -> IO (MockConversationMap, [Text])
loadRecordedConversation filePath = do
  s <- readMockRequestResponse filePath
  case s of
    Nothing -> error "can't read conversation"
    Just rrs ->
      let pairs = map (\rr -> (request rr, response rr)) rrs
          rrMap = M.fromList pairs
          prompts = rrs >>= \rsp -> case prompt rsp of Nothing -> []; Just p -> [p]
       in pure (rrMap, prompts)

mockProvider :: MockConversationMap -> LLMProvider -> LLMProvider
mockProvider mp adapter =
  adapter
    { sendRequest = \val -> do
        case M.lookup val mp of
          Nothing ->
            error (show val <> "\n" <> show (fst $ head $ M.toList mp))
          -- error ("value not found for:" <> show val)
          Just r -> pure (200, r),
      sendStreamRequest = \body _callback -> do
        case M.lookup body mp of
          Nothing ->
            -- error (show body <> "\n" <> show (fst $ head $ M.toList mp))
            error ("value not found for:" <> show body)
          Just r -> case parseMaybe parseChatResponse r of
            Nothing -> error "can't parse recorded ChatResponse json"
            Just chatResponse -> pure $ Right chatResponse
    }

streamChatLoopMain :: Bool -> Bool -> ChatEnv -> [Text] -> IO ()
streamChatLoopMain stream withInterp env prompts = do
  putStrLn "\n=== Ollama (with Claude  and Gemini fallbacks) ==="
  _ <- streamChatLoop stream withInterp env prompts
  pure ()

streamChatLoop :: Bool -> Bool -> ChatEnv -> [Text] -> IO Conversation
streamChatLoop stream withInterp env = aux emptyUsage (Conversation [])
  where
    streamIt = if withInterp then streamTextSimple else streamText
    generateIt = if withInterp then generateTextSimple else generateText
    aux _totalUsage conv [] = do
      return conv
    aux totalUsage conv (prompt : rest) = do
      -- firstChunkRef <- newIORef True
      result <- if stream then streamIt env conv prompt $ const (pure ()) else generateIt env conv prompt
      case result of
        Left (err, _, _) -> do
          print err
          pure conv
        Right (_, conv', usage) -> do
          aux (addUsage totalUsage usage) conv' rest
