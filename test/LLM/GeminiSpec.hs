module LLM.GeminiSpec (spec) where

import Data.Aeson (eitherDecodeFileStrict')
import LLM.Gemini (parseResponse, parseUsage)
import LLM.Types
import Test.Hspec

spec :: Spec
spec = describe "Gemini" $ do
  describe "parseResponse" $ do
    it "parses a text response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/gemini-text.json"
      resp <- parseResponse val
      case resp of
        Right r -> do
          respText r `shouldBe` "Hello! How can I help you today?"
          hasToolCalls r `shouldBe` False
        Left err -> expectationFailure $ "Parse failed: " <> show err

    it "parses a function call response" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/gemini-tool-use.json"
      resp <- parseResponse val
      case resp of
        Right r -> do
          hasToolCalls r `shouldBe` True
          let [tc] = getToolCalls r
          tcName tc `shouldBe` "get_weather"
        Left err -> expectationFailure $ "Parse failed: " <> show err

  describe "parseUsage" $ do
    it "extracts token counts" $ do
      Right val <- eitherDecodeFileStrict' "test/fixtures/gemini-text.json"
      parseUsage val `shouldBe` Just (Usage 20 8)
