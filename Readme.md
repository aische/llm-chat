# LLM-Chat

A library for building chat applications with LLMs.

## Status

This library is in an early stage: APIs and behavior may still change. It is work in progress, and test coverage is incomplete — some functionality is not yet covered by automated tests.

## Examples

```haskell
module Main where

import Data.Text.IO qualified as TIO
import LLM (createChatEnv, createModelConfig, emptyConversation, ollamaGateway)
import LLM.Generate.Generate (generateText)

main :: IO ()
main = do
    let mc = createModelConfig ollamaGateway "llama3.2"
        env = createChatEnv mc "" []

    result <- generateText env emptyConversation "what is the capital of france?"
    case result of
        Left e -> print e
        Right (answer, _, _) -> TIO.putStrLn answer
```

It includes a few basic tools and supports ollama, gemini, open-ai and claude:

```haskell
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Control.Retry (fullJitterBackoff, limitRetries)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM
  ( ChatEnv (envFallbacks, envHooks, envTools),
    LogLevel (Debug),
    ModelConfig (..),
    PricingInfo (..),
    claudeGateway,
    createChatEnv,
    emptyConversation,
    noHooks,
    openAIGateway,
    toTool,
    withJsonDump,
    withStderrLogger,
  )
import LLM.Generate.Generate (generateText)
import LLM.Tools.FsConfig (mkFsConfig)
import LLM.Tools.Readdir (readdirToolTyped)
import LLM.Tools.Readfile (readfileToolTyped)
import LLM.Tools.ReplaceInFile (replaceInFileToolTyped)
import LLM.Tools.Writefile (writefileToolTyped)
import System.Environment (getEnv)

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  claudeKey <- T.pack <$> getEnv "CLAUDE_API_KEY"
  openAIKey <- T.pack <$> getEnv "OPENAI_API_KEY"
  userProjectPath <- getEnv "USER_PROJECT_PATH"
  fsConfig <- mkFsConfig userProjectPath
  let claude = claudeGateway claudeKey
      openAI = openAIGateway openAIKey
      gpt_4_1 =
        ModelConfig
          { mcGateway = openAI,
            mcModel = "gpt-4.1-2025-04-14",
            mcPricing = PricingInfo {pricePerMillionInput = 2.0, pricePerMillionOutput = 8.0},
            mcMaxTokens = 1024,
            mcTemperature = Nothing,
            mcRequestTimeout = Nothing,
            mcThrottleDelay = Just 1_000,
            mcRetry = limitRetries 0 <> fullJitterBackoff 1_000_000
          }
      claude_haiku_4_5 =
        ModelConfig
          { mcGateway = claude,
            mcModel = "claude-haiku-4-5-20251001",
            mcPricing = PricingInfo {pricePerMillionInput = 1.0, pricePerMillionOutput = 5.00},
            mcMaxTokens = 1024,
            mcTemperature = Nothing,
            mcRequestTimeout = Nothing,
            mcThrottleDelay = Nothing,
            mcRetry = limitRetries 3 <> fullJitterBackoff 1_000_000
          }
      hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
      env =
        (createChatEnv gpt_4_1 "you are a friendly assistant." [])
          { envFallbacks = [claude_haiku_4_5],
            envTools =
              [ toTool $ readfileToolTyped fsConfig,
                toTool $ writefileToolTyped fsConfig,
                toTool $ replaceInFileToolTyped fsConfig,
                toTool $ readdirToolTyped fsConfig
              ],
            envHooks = hooks
          }
  result <- generateText env emptyConversation "create a document 'paris.md' with a poem about paris"
  case result of
    Left e -> print e
    Right (answer, _, _) -> TIO.putStrLn answer
```

Put your keys and user project path in .env:

```
GEMINI_API_KEY=<your gemini key>
CLAUDE_API_KEY=<your claude key>
OPENAI_API_KEY=<your openai key>
USER_PROJECT_PATH="path/to/user/project"
```

## Providers

- OpenAI
- Gemini
- Claude
- Ollama

## Basic llm functionality

- generateText
- streamText (with callback for each streamed chunk)
- generateObject (typed response)
- generateObjectUntyped (schema and result are Value objects)
- tools (typed)
- token counting / cost estimation
- hooks for logging and other side effects
- model fallback mechanism
