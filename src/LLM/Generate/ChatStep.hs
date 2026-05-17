module LLM.Generate.ChatStep
  ( ChatStep (..),
    buildChatStep,
    windowOffset,
  )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
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
    ToolCall,
    ToolResult,
    Turn (AssistantTurn, ToolTurn),
  )
import LLM.Core.Usage (Usage (..), addUsage, emptyUsage, estimateCost)
import LLM.Core.Utils (appendConversation, getToolCalls, hasToolCalls)
import LLM.Generate.Common
  ( mkRequestWithWorkers,
    requestLogMessage,
    responseLogMessage,
    toolCallsLogMessage,
    toolResultsLogMessage,
    windowOffset,
  )
import LLM.Generate.Types
  ( ChatEnv (..),
    GenerateText,
    ModelConfig (..),
    WorkerMap,
  )

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
buildChatStep ::
  (MonadUnliftIO m, MonadCatch m) =>
  Maybe (GenerateText m, WorkerMap m) ->
  ChatEnv m ->
  ModelConfig ->
  Int ->
  Usage ->
  Conversation ->
  ChatStep
buildChatStep mbGenWorkerMap env mc rounds acc conv
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
            let request = mkRequestWithWorkers mbGenWorkerMap env mc conv (envReadonly env)
             in Log Debug (requestLogMessage mc rounds request) $
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
                                 in Log Info (toolCallsLogMessage calls) $
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
                                              Log Debug (toolResultsLogMessage results) $
                                                let conv' = appendConversation conv [AssistantTurn (respText resp) calls, ToolTurn results]
                                                 in buildChatStep mbGenWorkerMap env mc (rounds + 1) acc' conv'
                                        }
                              else
                                Log Info (responseLogMessage resp) $
                                  let finalConv = Conversation (unConversation conv ++ [AssistantTurn (respText resp) []])
                                   in Done (Right (respText resp, finalConv, acc'))

-- Helpers --

maybeThrottle :: Maybe Int -> ChatStep -> ChatStep
maybeThrottle Nothing next = next
maybeThrottle (Just d) next = Throttle d next

tshow :: (Show a) => a -> Text
tshow = T.pack . show
