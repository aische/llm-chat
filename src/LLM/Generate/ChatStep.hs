module LLM.Generate.ChatStep
  ( ChatStep (..),
    buildChatStep,
    windowOffset,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Logger (LogLevel (..))
import LLM.Core.Types
  ( ChatRequest (..),
    ChatResponse (respText, respUsage),
    Conversation (..),
    LLMError (Aborted, ToolLoopExceeded),
    LLMTextResult,
    ToolCall (tcName),
    ToolResult (trContent, trName),
    Turn (AssistantTurn, ToolTurn),
  )
import LLM.Core.Usage (Usage (..), addUsage, emptyUsage, estimateCost)
import LLM.Core.Utils (getToolCalls, hasToolCalls)
import LLM.Generate.Types
  ( ChatEnv (..),
    ModelConfig (..),
  )
import LLM.Generate.Utils (mkRequest, windowOffset)

-- | A reified chat program. Each constructor is an effect the loop
-- needs, paired with a continuation that accepts the result.
-- The program is fully pure — interpreters decide how to execute effects.
data ChatStep
  = -- | Check whether the caller wants to abort.
    CheckAbort (Bool -> ChatStep)
  | -- | Emit a log message, then continue.
    Log LogLevel Text ChatStep
  | -- | Wait @n@ milliseconds, then continue.
    Throttle Int ChatStep
  | -- | Send a request to the LLM. The interpreter decides streaming
    -- vs non-streaming, retry policy, and timeout.
    CallLLM ChatRequest (LLMTextResult -> ChatStep)
  | -- | Execute tool calls. Carries enough context for interpreters
    -- to build a 'ToolContext' and to checkpoint/resume the session.
    ExecTools
      { esRound :: Int, -- current round number
        esCalls :: [ToolCall], -- tool calls to execute
        esRespText :: Text, -- assistant's text from this round
        esConv :: Conversation, -- conversation before this round
        esUsage :: Usage, -- accumulated usage including this LLM call
        esCont :: Either LLMError [ToolResult] -> ChatStep
      }
  | -- | Terminal: the loop is done.
    Done (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))

-- | Build a pure 'ChatStep' program from a 'ChatEnv' and 'ModelConfig'.
-- This replaces the old monolithic @chatLoop@ — same logic, zero IO.
buildChatStep :: ChatEnv -> ModelConfig -> Int -> Usage -> Conversation -> ChatStep
buildChatStep env mc rounds acc conv
  | rounds >= envMaxToolRounds env =
      Log Error ("Tool loop exceeded: " <> tshow rounds <> " rounds") $
        Done (Left (ToolLoopExceeded rounds, conv, acc))
  | otherwise =
      CheckAbort $ \aborted ->
        if aborted
          then
            Log Info "Aborted before API call" $
              Done (Left (Aborted, conv, acc))
          else
            let request = mkRequest env mc conv (envReadonly env)
             in Log Debug ("API request: model=" <> mcModel mc <> " round=" <> tshow rounds <> " turns=" <> tshow (length (unConversation $ reqConversation request))) $
                  maybeThrottle (mcThrottleDelay mc) $
                    CallLLM request $ \case
                      Left err ->
                        Log Error ("API error: " <> tshow err) $
                          Done (Left (err, conv, acc))
                      Right resp ->
                        let responseUsage = fromMaybe emptyUsage (respUsage resp)
                            cost = estimateCost (mcPricing mc) responseUsage
                            acc' = addUsage acc (responseUsage {usageTotalCost = cost})
                         in if hasToolCalls resp
                              then
                                let calls = getToolCalls resp
                                 in Log Info ("Tool calls: " <> T.intercalate ", " (map tcName calls)) $
                                      ExecTools
                                        { esRound = rounds,
                                          esCalls = calls,
                                          esRespText = respText resp,
                                          esConv = conv,
                                          esUsage = acc',
                                          esCont = \case
                                            Left _ ->
                                              Log Info "Aborted during tool execution" $
                                                Done (Left (Aborted, conv, acc'))
                                            Right results ->
                                              Log Debug ("Tool results: " <> T.intercalate ", " [trName r <> "=" <> T.take 100 (trContent r) | r <- results]) $
                                                let conv' =
                                                      Conversation
                                                        ( unConversation conv
                                                            ++ [AssistantTurn (respText resp) calls]
                                                            ++ [ToolTurn results]
                                                        )
                                                 in buildChatStep env mc (rounds + 1) acc' conv'
                                        }
                              else
                                Log Info (logResponse resp) $
                                  let finalConv = Conversation (unConversation conv ++ [AssistantTurn (respText resp) []])
                                   in Done (Right (respText resp, finalConv, acc'))

-- Helpers --

maybeThrottle :: Maybe Int -> ChatStep -> ChatStep
maybeThrottle Nothing next = next
maybeThrottle (Just d) next = Throttle d next

tshow :: (Show a) => a -> Text
tshow = T.pack . show

logResponse :: ChatResponse -> Text
logResponse resp =
  "Response: "
    <> T.take 100 (respText resp)
    <> maybe
      ""
      ( \u ->
          " usage="
            <> tshow (usageInputTokens u)
            <> "+"
            <> tshow (usageOutputTokens u)
      )
      (respUsage resp)
  where
    usageInputTokens = LLM.Core.Usage.usageInputTokens
    usageOutputTokens = LLM.Core.Usage.usageOutputTokens
