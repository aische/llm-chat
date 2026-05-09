module LLM.Generate.Utils
  ( defaultChatEnv,
    createChatEnv,
  )
where

import LLM.Generate.Types (ChatEnv (..), ModelConfig (..))
import Data.Text (Text)
import LLM.Core.Types (Tool)
import LLM.Core.Logger (noHooks)

-- | Sensible defaults — single model, no fallback.
defaultChatEnv :: ModelConfig -> ChatEnv
defaultChatEnv mc =
  ChatEnv
    { envModel = mc,
      envFallbacks = [],
      envSystem = Nothing,
      envTools = [],
      envMaxToolRounds = 10,
      envContextWindow = Nothing,
      envHooks = noHooks,
      envAbortSignal = Nothing
    }

createChatEnv :: ModelConfig -> Text -> [Tool] -> ChatEnv
createChatEnv mc system tools =
  ChatEnv
    { envModel = mc,
      envFallbacks = [],
      envSystem = Just system,
      envTools = tools,
      envMaxToolRounds = 10,
      envContextWindow = Nothing,
      envHooks = noHooks,
      envAbortSignal = Nothing
    }
