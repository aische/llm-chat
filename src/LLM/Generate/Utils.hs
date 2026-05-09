module LLM.Generate.Utils
  ( defaultChatEnv,
    createChatEnv,
  )
where

import Data.Text (Text)
import LLM.Core.Logger (noHooks)
import LLM.Core.Types (Tool)
import LLM.Generate.Types (ChatEnv (..), ModelConfig (..))

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
