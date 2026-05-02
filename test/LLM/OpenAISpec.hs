module LLM.OpenAISpec (spec) where

import Data.Aeson (eitherDecodeFileStrict')
import LLM.OpenAI (parseResponse, parseUsage)
import LLM.Types
import Test.Hspec

spec :: Spec
spec = describe "OpenAI" $ do
  describe "parseResponse" $ do
    it "parses a text response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-text.json"
      case parseResponse val of
        Right resp -> do
          respText resp `shouldBe` "Hello! How can I help you today?"
          hasToolCalls resp `shouldBe` False
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "parses a tool_calls response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-tool-use.json"
      case parseResponse val of
        Right resp -> do
          hasToolCalls resp `shouldBe` True
          let [tc] = getToolCalls resp
          tcName tc `shouldBe` "get_weather"
          tcId tc `shouldBe` "call_abc123"
        Left err -> expectationFailure $ "Parse failed: " <> show err

  describe "parseUsage" $ do
    it "extracts token counts" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/openai-text.json"
      parseUsage val `shouldBe` Just (Usage 15 9)
