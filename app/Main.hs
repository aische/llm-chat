{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Louter.Client (chatCompletion, defaultChatRequest)
import Louter.Client.Gemini (geminiClient)
import Louter.Types.Request (ContentPart (..), Message (..), MessageRole (..))
import Louter.Types.Response (ChatResponse (..), Choice (..))
import System.Environment (getEnv)

main :: IO ()
main = do
  loadFile defaultConfig
  apiKey <- T.pack <$> getEnv "GEMINI_API_KEY"
  client <- geminiClient apiKey

  let model = "gemini-2.5-flash"
      prompt = "Explain monads in one sentence, but make it funny."
      request =
        defaultChatRequest
          model
          [Message {msgRole = RoleUser, msgContent = [TextPart prompt]}]

  putStrLn $ "Asking: " <> T.unpack prompt
  putStrLn $ replicate 40 '-'

  result <- chatCompletion client request
  case result of
    Left err -> putStrLn $ "Error: " <> T.unpack err
    Right resp ->
      case respChoices resp of
        [] -> putStrLn "(no response)"
        (c : _) -> TIO.putStrLn (choiceMessage c)
