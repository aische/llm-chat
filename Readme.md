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
- session chat
- non-interactive command line chat interface, persists conversation history in a local file

## TODO

- some tests use fake data that should be replaced with real data in the fixtures. 

- abort was added but not used or tested at all
