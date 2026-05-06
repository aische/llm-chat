# LLM-Chat

A library for building chat applications with LLMs.

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

## Frontends

- chat repl
- non-interactive command line chat interface, persists conversation history in a local file

## TODO

- tests use fake data that should be replaced with real data in the fixtures. Real data can be obtained from the dumps when withJsonDump is applied to the logging hooks.
- most code is not tested at all

- abort was added but not used or tested at all
