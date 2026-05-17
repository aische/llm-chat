# LLM-Chat

A library for building chat applications with LLMs.

## Status

This library is in an early stage: APIs and behavior may still change. It is work in progress, and test coverage is incomplete. Currently there are 2 different implementations of generateText and streamText (generateTextSimple and streamTextSimple), the 'simple' ones use a free monad pattern.

Tools:
- read_file
- write_file
- read_dir
- directory_tree
- replace_in_file
- multi_replace_in_file
- copy_file
- create_directory
- move_file
- remove_directory
- remove_file

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
module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
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
            mcRetryCount = 0,
            mcJitterBackoff = 1_000
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
            mcRetryCount = 0,
            mcJitterBackoff = 1_000
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
    - Readfile
    - Writefile
    - ReplaceInFile
    - Readdir
- token counting / cost estimation
- hooks for logging and other side effects
- model fallback mechanism

## executable

- simple repl

```
> llm-chat repl
```

- simple session based chat

```
> llm-chat prompt "what is the capital of France?"
$ "The capital of France is Faris"
> llm-chat prompt "and Germany?"
$ "It's Berlin"
> llm-chat show
$ ... shows the full conversation ...
> llm-chat clear
```

## Config files

Instead of hardcoding the model configs and the chat envs, they can live in JSON files:

`model-catalog.json`:

```json
[
    {
        "modelConfigName": "gpt_4_1",
        "providerName": "openai",
        "modelName": "gpt-4.1-2025-04-14",
        "pricing": {
            "pricePerMillionInput": 2.0,
            "pricePerMillionOutput": 8.0
        },
        "maxTokens": 1024,
        "temperature": 0.5,
        "requestTimeout": 10000,
        "throttleDelay": 1000,
        "retryCount": 3,
        "jitterBackoff": 1000
    },
    {
        "modelConfigName": "llama_3_2",
        "providerName": "ollama",
        "modelName": "llama3.2:latest",
        "pricing": {
            "pricePerMillionInput": 0.0,
            "pricePerMillionOutput": 0.0
        },
        "maxTokens": 1024,
        "temperature": 0.5,
        "requestTimeout": 10000,
        "throttleDelay": 1000,
        "retryCount": 3,
        "jitterBackoff": 1000
    },
    {
        "modelConfigName": "gemini_2_5_flash",
        "providerName": "gemini",
        "modelName": "gemini-2.5-flash",
        "pricing": {
            "pricePerMillionInput": 0.1,
            "pricePerMillionOutput": 0.4
        },
        "maxTokens": 1024,
        "temperature": 0.5,
        "requestTimeout": 10000,
        "throttleDelay": 1000,
        "retryCount": 3,
        "jitterBackoff": 1000
    },
    {
        "modelConfigName": "gemini_lite",
        "providerName": "gemini",
        "modelName": "gemini-3.1-flash-lite",
        "pricing": {
            "pricePerMillionInput": 0.1,
            "pricePerMillionOutput": 0.4
        },
        "maxTokens": 1024,
        "temperature": 0.5,
        "requestTimeout": 10000,
        "throttleDelay": 1000,
        "retryCount": 3,
        "jitterBackoff": 1000
    }
]
```

`chat-env-catalog.json`:

```json
[
    {
        "chatEnvName": "default",
        "model": "llama_3_2",
        "fallbacks": ["gemini_lite"],
        "systemPrompt": "You are a helpful assistant who answers questions and executes tools for the user. Always use tools when asked to.",
        "tools": ["read_file", "read_dir", "write_file"],
        "maximumToolRounds": 3,
        "contextWindowSize": 3
    },
    {
        "chatEnvName": "funny",
        "model": "gpt_4_1",
        "fallbacks": ["llama_3_2"],
        "systemPrompt": "You are funny assistand and answer in an funny and friendly way",
        "tools": [],
        "maximumToolRounds": 3,
        "contextWindowSize": 3
    },
    {
        "chatEnvName": "angry",
        "model": "gpt_4_1",
        "fallbacks": ["llama_3_2"],
        "systemPrompt": "You are unfriendly asistant and answer in an angry way",
        "tools": [],
        "maximumToolRounds": 3,
        "contextWindowSize": 3
    }
]
```

example that uses the config files:

```haskell
module Example4 where

import LLM (LogLevel (Debug), generateText, noHooks, withJsonDump, withStderrLogger)
import LLM.Core.Utils (emptyConversation)
import LLM.Generate.LoadModels (loadEnvsOrThrow)

main :: IO ()
main = do
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
  (funnyEnv, angryEnv) <- loadEnvsOrThrow hooks ("funny", "angry")
  r <- generateText funnyEnv emptyConversation "what is the capital of france?"
  print r
  r2 <- generateText angryEnv emptyConversation "what is the capital of france?"
  print r2
```

## Example 5

run `chat-llm example5`

and write the following prompt: "give me summaries of all files in the workspace"

(workspace directory is defined in .env)

Use configs like this:

`model-catalog.json`:

```json
[
    {
        "modelConfigName": "gpt_4_1",
        "providerName": "openai",
        "modelName": "gpt-4.1-2025-04-14",
        "pricing": {
            "pricePerMillionInput": 2.0,
            "pricePerMillionOutput": 8.0
        },
        "maxTokens": 1024,
        "temperature": 0.5,
        "requestTimeout": 10000,
        "throttleDelay": 1000,
        "retryCount": 3,
        "jitterBackoff": 1000
    },
    {
        "modelConfigName": "haiku_4_5",
        "providerName": "claude",
        "modelName": "claude-haiku-4-5-20251001",
        "pricing": {
            "pricePerMillionInput": 1,
            "pricePerMillionOutput": 5
        },
        "maxTokens": 1024,
        "temperature": 0.5,
        "requestTimeout": 10000,
        "throttleDelay": 1000,
        "retryCount": 3,
        "jitterBackoff": 1000
    }
]
```

`chat-env-catalog.json`:

```json
[
    {
        "chatEnvName": "orchestrator",
        "model": "gpt_4_1",
        "fallbacks": [],
        "systemPrompt": "You are an orchestrator who coordinates the work of the workers",
        "tools": ["read_dir"],
        "workers": ["summarizer"],
        "maximumToolRounds": 10,
        "contextWindowSize": 3
    },
    {
        "chatEnvName": "summarize-env",
        "model": "haiku_4_5",
        "fallbacks": [],
        "systemPrompt": "file:summarizer-prompt.md",
        "tools": ["read_file"],
        "maximumToolRounds": 10,
        "contextWindowSize": 3
    }
]
```

`worker-catalog.json`:

```json
[
    {
        "name": "summarizer",
        "env": "summarize-env",
        "description": "A worker that can summarize files if provided with a filepath. For example, ask 'summarize the file somefilename.txt' "
    }
]
```

and `summarizer-prompt.md`:

```
You are a summarizer who summarizes files. You will be provided with a filepath and you will need to summarize the file. Respond with plain text.
```
