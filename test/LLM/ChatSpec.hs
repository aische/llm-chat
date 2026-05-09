{-# OPTIONS_GHC -Wno-missing-fields #-}

module LLM.ChatSpec (spec) where

import Control.Retry (limitRetries)
import Data.Aeson (object, (.=))
import LLM.Core.Abort (abort, newAbortSignal)
import LLM.Core.Types
  ( ChatRequest (reqConversation),
    ChatResponse (ChatResponse),
    ContentBlock (TextBlock, ToolCallBlock),
    Conversation (..),
    LLMError (Aborted, HttpError, NetworkError, ToolLoopExceeded),
    LLMGateway (..),
    Tool (..),
    ToolCall (ToolCall),
    ToolDef (ToolDef, toolDescription, toolName, toolParameters),
    Turn (ToolTurn),
  )
import LLM.Core.Usage (PricingInfo (..), Usage (Usage))
import LLM.Generate.Generate
  ( generateText,
  )
import LLM.Generate.Types
  ( ChatEnv (..),
    ModelConfig (..),
  )
import LLM.Generate.Utils (defaultChatEnv)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
  )

-- | A mock gateway that returns a fixed response
mockGateway :: ChatResponse -> LLMGateway
mockGateway resp =
  LLMGateway
    { gwName = "mock",
      gwGenerateText = \_ _ -> pure (Right resp),
      gwStreamText = \_ _ _ -> pure (Right resp)
    }

-- | A mock gateway that returns an error
mockErrorGateway :: LLMError -> LLMGateway
mockErrorGateway err =
  LLMGateway
    { gwName = "mock-error",
      gwGenerateText = \_ _ -> pure (Left err),
      gwStreamText = \_ _ _ -> pure (Left err)
    }

-- | A mock gateway that calls a tool, then responds with text
mockToolGateway :: LLMGateway
mockToolGateway =
  LLMGateway
    { gwName = "mock-tool",
      gwGenerateText = \_ req ->
        if any isToolTurn (unConversation $ reqConversation req)
          then pure $ Right (ChatResponse "The weather is sunny." [TextBlock "The weather is sunny."] (Just (Usage 80 15 0)))
          else
            let tc = ToolCall "call_1" "get_weather" (object ["location" .= ("London" :: String)])
             in pure $ Right (ChatResponse "" [ToolCallBlock tc] (Just (Usage 50 10 0))),
      gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing)
    }
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

zeroPricing :: PricingInfo
zeroPricing = PricingInfo 0 0

-- | Wrap a gateway in a ModelConfig with test defaults
mockModel :: LLMGateway -> ModelConfig
mockModel gw =
  ModelConfig
    { mcGateway = gw,
      mcModel = "test-model",
      mcPricing = zeroPricing,
      mcMaxTokens = 1024,
      mcTemperature = Nothing,
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetry = limitRetries 0
    }

weatherTool :: Tool
weatherTool =
  Tool
    { toolDef =
        ToolDef
          { toolName = "get_weather",
            toolDescription = "Get weather",
            toolParameters = object ["type" .= ("object" :: String)]
          },
      toolExecute = \_ _ -> pure "Sunny, 22°C"
    }

env :: LLMGateway -> ChatEnv
env gw = defaultChatEnv (mockModel gw)

spec :: Spec
spec = describe "Chat" $ do
  describe "generateText" $ do
    it "returns text for a simple response" $ do
      let gw = mockGateway (ChatResponse "Hi there!" [TextBlock "Hi there!"] (Just (Usage 10 5 0)))
      result <- generateText (env gw) (Conversation []) "hello"
      case result of
        Right (text, conv, usage) -> do
          text `shouldBe` "Hi there!"
          length (unConversation conv) `shouldBe` 2 -- UserTurn + AssistantTurn
          usage `shouldBe` Usage 10 5 0
        Left err -> expectationFailure $ show err

    it "propagates errors" $ do
      let gw = mockErrorGateway (HttpError 500 "internal error")
      result <- generateText (env gw) (Conversation []) "hello"
      case result of
        Left (HttpError 500 _, _, _) -> pure ()
        other -> expectationFailure $ "Expected HttpError 500, got: " <> show other

    it "handles tool call loop" $ do
      let e = (env mockToolGateway) {envTools = [weatherTool]}
      result <- generateText e (Conversation []) "weather in london?"
      case result of
        Right (text, conv, usage) -> do
          text `shouldBe` "The weather is sunny."
          -- UserTurn + AssistantTurn(tool call) + ToolTurn + AssistantTurn(final)
          length (unConversation conv) `shouldBe` 4
          usage `shouldBe` Usage 130 25 0 -- 50+80 input, 10+15 output
        Left err -> expectationFailure $ show err

    it "respects maxToolRounds" $ do
      -- A gateway that always returns tool calls
      let infiniteToolGateway =
            LLMGateway
              { gwName = "mock-infinite",
                gwGenerateText = \_ _ ->
                  let tc = ToolCall "call_1" "get_weather" (object [])
                   in pure $ Right (ChatResponse "" [ToolCallBlock tc] Nothing),
                gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing)
              }
          limitedEnv = (env infiniteToolGateway) {envMaxToolRounds = 2, envTools = [weatherTool]}
      result <- generateText limitedEnv (Conversation []) "test"
      case result of
        Left (ToolLoopExceeded 2, _, _) -> pure ()
        other -> expectationFailure $ "Expected ToolLoopExceeded 2, got: " <> show other

    it "falls back to next model on retryable error" $ do
      let failGw = mockErrorGateway (HttpError 503 "service unavailable")
          okGw = mockGateway (ChatResponse "Fallback worked!" [TextBlock "Fallback worked!"] (Just (Usage 10 5 0)))
          e = (defaultChatEnv (mockModel failGw)) {envFallbacks = [mockModel okGw]}
      result <- generateText e (Conversation []) "hello"
      case result of
        Right (text, _, _) -> text `shouldBe` "Fallback worked!"
        Left err -> expectationFailure $ "Expected fallback success, got: " <> show err

    it "falls back on non-retryable error too" $ do
      let failGw = mockErrorGateway (HttpError 400 "bad request")
          okGw = mockGateway (ChatResponse "Fallback worked!" [TextBlock "Fallback worked!"] (Just (Usage 10 5 0)))
          e = (defaultChatEnv (mockModel failGw)) {envFallbacks = [mockModel okGw]}
      result <- generateText e (Conversation []) "hello"
      case result of
        Right (text, _, _) -> text `shouldBe` "Fallback worked!"
        Left err -> expectationFailure $ "Expected fallback success, got: " <> show err

    it "returns error from last model when all fail" $ do
      let failGw1 = mockErrorGateway (HttpError 503 "service unavailable")
          failGw2 = mockErrorGateway (HttpError 400 "bad request")
          e = (defaultChatEnv (mockModel failGw1)) {envFallbacks = [mockModel failGw2]}
      result <- generateText e (Conversation []) "hello"
      case result of
        Left (HttpError 400 _, _, _) -> pure ()
        other -> expectationFailure $ "Expected HttpError 400 from last model, got: " <> show other

    it "returns Aborted when signal is fired before the call" $ do
      let gw = mockGateway (ChatResponse "Hi!" [TextBlock "Hi!"] Nothing)
      sig <- newAbortSignal
      abort sig
      let e = (env gw) {envAbortSignal = Just sig}
      result <- generateText e (Conversation []) "hello"
      case result of
        Left (Aborted, _, _) -> pure ()
        other -> expectationFailure $ "Expected Aborted, got: " <> show other

    it "returns Aborted during tool execution" $ do
      sig <- newAbortSignal
      let slowTool =
            Tool
              { toolDef =
                  ToolDef
                    { toolName = "slow",
                      toolDescription = "A slow tool",
                      toolParameters = object ["type" .= ("object" :: String)]
                    },
                toolExecute = \_ _ -> do
                  abort sig -- abort while executing
                  pure "done"
              }
          -- gateway that always asks for two tool calls
          twoCallGw =
            LLMGateway
              { gwName = "mock-two",
                gwGenerateText = \_ _ ->
                  let tc1 = ToolCall "c1" "slow" (object [])
                      tc2 = ToolCall "c2" "slow" (object [])
                   in pure $ Right (ChatResponse "" [ToolCallBlock tc1, ToolCallBlock tc2] Nothing),
                gwStreamText = \_ _ _ -> pure $ Right (ChatResponse "" [] Nothing)
              }
          e = (env twoCallGw) {envTools = [slowTool], envAbortSignal = Just sig}
      result <- generateText e (Conversation []) "go"
      case result of
        Left (Aborted, _, _) -> pure ()
        other -> expectationFailure $ "Expected Aborted during tools, got: " <> show other

    it "does not fall back on Aborted" $ do
      let gw = mockGateway (ChatResponse "Hi!" [TextBlock "Hi!"] Nothing)
          okGw = mockGateway (ChatResponse "Fallback" [TextBlock "Fallback"] Nothing)
      sig <- newAbortSignal
      abort sig
      let e = (defaultChatEnv (mockModel gw)) {envFallbacks = [mockModel okGw], envAbortSignal = Just sig}
      result <- generateText e (Conversation []) "hello"
      case result of
        Left (Aborted, _, _) -> pure ()
        other -> expectationFailure $ "Expected Aborted (no fallback), got: " <> show other
