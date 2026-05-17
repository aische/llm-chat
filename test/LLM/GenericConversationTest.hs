module LLM.GenericConversationTest (createSpec, GenericConversationTextOps (..)) where

import Data.Text qualified as T
import LLM.Core.LLMProvider (LLMProvider, toGateway)
import LLM.Core.Types (Conversation (Conversation))
import LLM.Core.Usage (PricingInfo (..))
import LLM.Core.Utils (toTool)
import LLM.Generate.Types
  ( ChatEnv (..),
    ModelConfig (..),
  )
import LLM.Generate.Utils (createChatEnv)
import LLM.TestKit
  ( loadRecordedConversation,
    mockProvider,
    streamChatLoop,
  )
import LLM.Tools.Weather (weatherToolTyped)
import Test.Hspec (Spec, describe, it, shouldBe)

data GenericConversationTextOps = GenericConversationTextOps
  { specTitle :: String,
    specProvider :: LLMProvider,
    modelName :: String,
    filePathGenerated :: String,
    filePathStreamed :: String
  }

createSpec :: GenericConversationTextOps -> Spec
createSpec opts = describe (specTitle opts) $ do
  it "generateText" $ do
    (m, p) <- loadRecordedConversation (filePathGenerated opts)
    let provider = toGateway $ mockProvider m (specProvider opts)
        modelConf =
          ModelConfig
            { mcGateway = provider,
              mcModel = T.pack $ modelName opts,
              mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Nothing,
              mcRetryCount = 3,
              mcJitterBackoff = 1_000
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
    (m, p) <- loadRecordedConversation (filePathStreamed opts)
    let provider = toGateway $ mockProvider m (specProvider opts)
        modelConf =
          ModelConfig
            { mcGateway = provider,
              mcModel = T.pack $ modelName opts,
              mcPricing = PricingInfo {pricePerMillionInput = 0.0, pricePerMillionOutput = 0.0},
              mcMaxTokens = 1024,
              mcTemperature = Nothing,
              mcRequestTimeout = Nothing,
              mcThrottleDelay = Nothing,
              mcRetryCount = 3,
              mcJitterBackoff = 1_000
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
