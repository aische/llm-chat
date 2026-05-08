module LLM.Claude2Spec (spec) where

import Control.Retry (fullJitterBackoff, limitRetries)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import LLM (createChatEnv, toProvider, toTool)
import LLM.Core.LLMProvider (ChatEnv (..), ModelConfig (..))
import LLM.Core.Types
import LLM.Core.Usage (PricingInfo (..), Usage (..))
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Providers.Claude (parseClaudeResponse, parseClaudeUsage)
import LLM.Providers.Ollama (ollama)
import LLM.TestKit
import LLM.Tools.Weather (weatherToolTyped)
import Test.Hspec

ollamaConversationGeneratedFilePath :: String
ollamaConversationGeneratedFilePath = "./test/fixtures/ollama-conversation-generated.json"

ollamaConversationStreamedFilePath :: String
ollamaConversationStreamedFilePath = "./test/fixtures/ollama-conversation-streamed.json"

spec :: Spec
spec = describe "Claude" $ do
  describe "recorded conversation" $ do
    it "ollama generateText" $ do
      (m, p) <- loadRecordedConversation ollamaConversationGeneratedFilePath
      let provider = toProvider $ mockProvider m ollama
          modelConf =
            ModelConfig
              { mcProvider = provider,
                mcModel = "llama3.2:latest",
                mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
                mcMaxTokens = 1024,
                mcTemperature = Nothing,
                mcRequestTimeout = Nothing,
                mcThrottleDelay = Nothing,
                mcRetry = limitRetries 3 <> fullJitterBackoff 1_000_000
              }
          systemPrompt = "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to, but use only the tools that are available."
          env =
            ( createChatEnv
                modelConf
                systemPrompt
                []
            )
              { envTools = [toTool weatherToolTyped]
              }

      Conversation turns <- streamChatLoop False env p
      length turns `shouldBe` 8
    it "ollama streamText" $ do
      (m, p) <- loadRecordedConversation ollamaConversationStreamedFilePath
      let provider = toProvider $ mockProvider m ollama
          modelConf =
            ModelConfig
              { mcProvider = provider,
                mcModel = "llama3.2:latest",
                mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
                mcMaxTokens = 1024,
                mcTemperature = Nothing,
                mcRequestTimeout = Nothing,
                mcThrottleDelay = Nothing,
                mcRetry = limitRetries 3 <> fullJitterBackoff 1_000_000
              }
          systemPrompt = "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to, but use only the tools that are available."
          env =
            ( createChatEnv
                modelConf
                systemPrompt
                []
            )
              { envTools = [toTool weatherToolTyped]
              }

      Conversation turns <- streamChatLoop True env p
      length turns `shouldBe` 8
