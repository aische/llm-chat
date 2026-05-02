module LLM.ChatSpec (spec) where

import Data.Aeson (object, (.=))
import LLM.Chat (runChat)
import LLM.Types
import Test.Hspec

-- | A mock client that returns a fixed response
mockClient :: ChatResponse -> LLMClient
mockClient resp =
  LLMClient
    { clientChat = \_ -> pure (Right resp),
      clientChatStream = Nothing
    }

-- | A mock client that returns an error
mockErrorClient :: LLMError -> LLMClient
mockErrorClient err =
  LLMClient
    { clientChat = \_ -> pure (Left err),
      clientChatStream = Nothing
    }

-- | A mock client that calls a tool, then responds with text
mockToolClient :: LLMClient
mockToolClient =
  LLMClient
    { clientChat = \req ->
        if any isToolTurn (reqConversation req)
          then pure $ Right (ChatResponse "The weather is sunny." [TextBlock "The weather is sunny."] (Just (Usage 80 15)))
          else
            let tc = ToolCall "call_1" "get_weather" (object ["location" .= ("London" :: String)])
             in pure $ Right (ChatResponse "" [ToolCallBlock tc] (Just (Usage 50 10))),
      clientChatStream = Nothing
    }
  where
    isToolTurn (ToolTurn _) = True
    isToolTurn _ = False

weatherTool :: Tool
weatherTool =
  Tool
    { toolDef =
        ToolDef
          { toolName = "get_weather",
            toolDescription = "Get weather",
            toolParameters = object ["type" .= ("object" :: String)]
          },
      toolExecute = \_ -> pure "Sunny, 22°C"
    }

cfg :: ChatConfig
cfg = (defaultChatConfig "test-model") {cfgRetry = noRetry}

spec :: Spec
spec = describe "Chat" $ do
  describe "runChat" $ do
    it "returns text for a simple response" $ do
      let client = mockClient (ChatResponse "Hi there!" [TextBlock "Hi there!"] (Just (Usage 10 5)))
      result <- runChat client cfg [] [] "hello"
      case result of
        Right (text, conv, usage) -> do
          text `shouldBe` "Hi there!"
          length conv `shouldBe` 2 -- UserTurn + AssistantTurn
          usage `shouldBe` Usage 10 5
        Left err -> expectationFailure $ show err

    it "propagates errors" $ do
      let client = mockErrorClient (HttpError 500 "internal error")
      result <- runChat client cfg [] [] "hello"
      case result of
        Left (HttpError 500 _) -> pure ()
        other -> expectationFailure $ "Expected HttpError 500, got: " <> show other

    it "handles tool call loop" $ do
      result <- runChat mockToolClient cfg [weatherTool] [] "weather in london?"
      case result of
        Right (text, conv, usage) -> do
          text `shouldBe` "The weather is sunny."
          -- UserTurn + AssistantTurn(tool call) + ToolTurn + AssistantTurn(final)
          length conv `shouldBe` 4
          usage `shouldBe` Usage 130 25 -- 50+80 input, 10+15 output
        Left err -> expectationFailure $ show err

    it "respects maxToolRounds" $ do
      -- A client that always returns tool calls
      let infiniteToolClient =
            LLMClient
              { clientChat = \_ ->
                  let tc = ToolCall "call_1" "get_weather" (object [])
                   in pure $ Right (ChatResponse "" [ToolCallBlock tc] Nothing),
                clientChatStream = Nothing
              }
          limitedCfg = cfg {cfgMaxToolRounds = 2}
      result <- runChat infiniteToolClient limitedCfg [weatherTool] [] "test"
      case result of
        Left (ToolLoopExceeded 2) -> pure ()
        other -> expectationFailure $ "Expected ToolLoopExceeded 2, got: " <> show other
