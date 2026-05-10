module LLM.Generate.Utils
  ( defaultChatEnv,
    createChatEnv,
    createModelConfig,
    windowOffset,
    findNthUserFromEnd,
  )
where

import Control.Retry (fullJitterBackoff, limitRetries)
import Data.Text (Text)
import LLM.Core.Logger (noHooks)
import LLM.Core.Types (Conversation (unConversation), LLMGateway, Tool, Turn (UserTurn))
import LLM.Core.Usage (PricingInfo (..))
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

createModelConfig :: LLMGateway -> Text -> ModelConfig
createModelConfig gateway modelName =
  ModelConfig
    { mcGateway = gateway,
      mcModel = modelName,
      mcPricing = PricingInfo {pricePerMillionInput = 0, pricePerMillionOutput = 0},
      mcMaxTokens = 1024,
      mcTemperature = Nothing,
      mcRequestTimeout = Nothing,
      mcThrottleDelay = Nothing,
      mcRetry = limitRetries 0 <> fullJitterBackoff 1_000_000
    }

-- | Compute the index where the visible window starts.
-- The window includes the last @n@ user messages and all turns that follow
-- each of them (assistant replies, tool rounds, etc.).
-- Returns 0 (no windowing) when the window is 'Nothing' or the conversation
-- contains fewer than @n@ user messages.
windowOffset :: Maybe Int -> Conversation -> Int
windowOffset Nothing _ = 0
windowOffset (Just n) conv = findNthUserFromEnd n conv

-- | Find the index of the Nth 'UserTurn' from the end of a conversation.
-- Returns 0 if there are fewer than @n@ user messages.
findNthUserFromEnd :: Int -> Conversation -> Int
findNthUserFromEnd 0 _conv = 0
findNthUserFromEnd n conv = go (length (unConversation conv) - 1) n
  where
    go idx remaining
      | idx < 0 = 0
      | remaining <= 0 = idx + 1
      | otherwise = case unConversation conv !! idx of
          UserTurn _ -> go (idx - 1) (remaining - 1)
          _ -> go (idx - 1) remaining
