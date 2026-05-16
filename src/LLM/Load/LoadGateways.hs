module LLM.Load.LoadGateways where

import Control.Lens ((<&>))
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import LLM.Load.Types (GatewayMap)
import LLM.Providers.Claude (claudeGateway)
import LLM.Providers.Gemini (geminiGateway)
import LLM.Providers.Ollama (ollamaGateway)
import LLM.Providers.OpenAI (openAIGateway)
import System.Environment (lookupEnv)

loadGateways :: IO GatewayMap
loadGateways = do
  let ollama = Just ("ollama", ollamaGateway)
  openai <- lookupEnv "OPENAI_API_KEY" <&> fmap (("openai",) . openAIGateway . T.pack)
  claude <- lookupEnv "CLAUDE_API_KEY" <&> fmap (("claude",) . claudeGateway . T.pack)
  gemini <- lookupEnv "GEMINI_API_KEY" <&> fmap (("gemini",) . geminiGateway . T.pack)
  pure $ Map.fromList $ catMaybes [openai, claude, gemini, ollama]
