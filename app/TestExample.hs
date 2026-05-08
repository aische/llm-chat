{-# OPTIONS_GHC -Wall #-}

module TestExample where

import Adapters.StreamChatLoop (streamChatLoopMain)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import LLM.Core.Generate (ChatEnv (..), ModelConfig, createChatEnv)
import LLM.Core.Logger
  ( LogLevel (..),
    noHooks,
    withJsonDump,
    withStderrLogger,
  )
import LLM.Core.Utils (toTool)
import LLM.Tools.Weather (weatherToolTyped)

testExample :: Bool -> ModelConfig -> IO ()
testExample stream model = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
      systemPrompt = "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to, but use only the tools that are available."
      tools =
        [ toTool weatherToolTyped
        ]
      env =
        (createChatEnv model systemPrompt [])
          { envHooks = hooks,
            envContextWindow = Just 3,
            envTools = tools
          }
  streamChatLoopMain stream env

{-

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  AllModels {gemini_2_5_flash, claude_haiku_4_5, llama_3_2, gpt_4_1, gpt_5_nano} <- getAllModels
  testExample False llama_3_2

-- do this for all providers and for stream/generate
-- then put the request/response pairs (and additional prompt if appropriate) into fixture file

-}