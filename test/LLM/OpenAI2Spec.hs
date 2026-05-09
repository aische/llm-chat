module LLM.OpenAI2Spec (spec) where

import Control.Retry (fullJitterBackoff, limitRetries)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import LLM (createChatEnv, toGateway, toTool)
import LLM.Core.Generate (ChatEnv (..), ModelConfig (..))
import LLM.Core.Types
import LLM.Core.Usage (PricingInfo (..), Usage (..))
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Providers.OpenAI (openAIProvider)
import LLM.TestKit
import LLM.Tools.Weather (weatherToolTyped)
import Test.Hspec

openAIConversationGeneratedFilePath :: String
openAIConversationGeneratedFilePath = "./test/fixtures/openai-conversation-generated.json"

openAIConversationStreamedFilePath :: String
openAIConversationStreamedFilePath = "./test/fixtures/openai-conversation-streamed.json"

-- ollamaConversationStreamedFilePath :: String
-- ollamaConversationStreamedFilePath = "./test/fixtures/ollama-conversation-streamed.json"

spec :: Spec
spec = describe "OpenAI" $ do
  describe "recorded conversation" $ do
    it "generateText" $ do
      (m, p) <- loadRecordedConversation openAIConversationGeneratedFilePath
      let provider = toGateway $ mockProvider m (openAIProvider "")
          modelConf =
            ModelConfig
              { mcGateway = provider,
                mcModel = "gpt-4.1-2025-04-14",
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

    it "streamText" $ do
      (m, p) <- loadRecordedConversation openAIConversationStreamedFilePath
      let provider = toGateway $ mockProvider m (openAIProvider "")
          modelConf =
            ModelConfig
              { mcGateway = provider,
                mcModel = "gpt-4.1-2025-04-14",
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
