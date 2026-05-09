module LLM.RecordedConversationSpec (spec) where

import Control.Retry (fullJitterBackoff, limitRetries)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import LLM (createChatEnv, geminiProvider, ollama, openAIProvider, toGateway, toTool)
import LLM.Core.Generate (ChatEnv (..), ModelConfig (..))
import LLM.Core.Types
import LLM.Core.Usage (PricingInfo (..), Usage (..))
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.GenericConversationTest (GenericConversationTextOps (..), createSpec)
import LLM.Providers.Claude (claudeProvider)
import LLM.TestKit
import LLM.Tools.Weather (weatherToolTyped)
import Test.Hspec

ollamaConversationGeneratedFilePath :: String
ollamaConversationGeneratedFilePath = "./test/fixtures/ollama-conversation-generated.json"

ollamaConversationStreamedFilePath :: String
ollamaConversationStreamedFilePath = "./test/fixtures/ollama-conversation-streamed.json"

claudeConversationGeneratedFilePath :: String
claudeConversationGeneratedFilePath = "./test/fixtures/claude-conversation-generated.json"

claudeConversationStreamedFilePath :: String
claudeConversationStreamedFilePath = "./test/fixtures/claude-conversation-streamed.json"

geminiConversationGeneratedFilePath :: String
geminiConversationGeneratedFilePath = "./test/fixtures/gemini-conversation-generated.json"

geminiConversationStreamedFilePath :: String
geminiConversationStreamedFilePath = "./test/fixtures/gemini-conversation-streamed.json"

openAIConversationGeneratedFilePath :: String
openAIConversationGeneratedFilePath = "./test/fixtures/openai-conversation-generated.json"

openAIConversationStreamedFilePath :: String
openAIConversationStreamedFilePath = "./test/fixtures/openai-conversation-streamed.json"

spec =
  describe "recorded conversation" $ do
    createSpec $
      GenericConversationTextOps
        "Ollama"
        ollama
        "llama3.2:latest"
        "./test/fixtures/ollama-conversation-generated.json"
        "./test/fixtures/ollama-conversation-streamed.json"
    createSpec $
      GenericConversationTextOps
        "Claudess"
        (claudeProvider "")
        "claude-haiku-4-5-20251001"
        "./test/fixtures/claude-conversation-generated.json"
        "./test/fixtures/claude-conversation-streamed.json"
    createSpec $
      GenericConversationTextOps
        "Gemini"
        (geminiProvider "")
        "gemini-2.5-flash"
        "./test/fixtures/gemini-conversation-generated.json"
        "./test/fixtures/gemini-conversation-streamed.json"
    createSpec $
      GenericConversationTextOps
        "OpenAI"
        (openAIProvider "")
        "gpt-4.1-2025-04-14"
        "./test/fixtures/openai-conversation-generated.json"
        "./test/fixtures/openai-conversation-streamed.json"
