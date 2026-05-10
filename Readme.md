work in progress

# LLM-Chat

A library for building chat applications with LLMs.

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
