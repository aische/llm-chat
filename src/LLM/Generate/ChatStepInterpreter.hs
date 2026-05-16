module LLM.Generate.ChatStepInterpreter
  ( generateTextWith,
    streamTextWith,
    ChatStepInterpreter,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Retry (RetryPolicyM)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Abort (AbortSignal)
import LLM.Core.Logger (Hooks (..), LogLevel (..), safeHooks)
import LLM.Core.Types
  ( ChatRequest,
    Conversation (..),
    LLMError (..),
    LLMGateway (..),
    LLMTextResult,
    StreamEvent,
    Tool,
    Turn (..),
  )
import LLM.Core.Usage (Usage)
import LLM.Generate.ChatStep (ChatStep (..), buildChatStep)
import LLM.Generate.Common (getFilteredToolsWithWorkers, modelRetryPolicy)
import LLM.Generate.Types
  ( ChatEnv (..),
    GenerateText,
    ModelConfig (..),
    WorkerMap,
  )
import LLM.Generate.WithFallback (withFallback)

-- | A step interpreter runs a 'ChatStep' program to completion.
-- Both 'runStepIO' and @runStepServer store sid@ satisfy this type.
type ChatStepInterpreter m =
  Hooks ->
  Maybe AbortSignal ->
  [Tool] ->
  Maybe Int -> -- context window
  RetryPolicyM IO ->
  Maybe Int -> -- request timeout
  (ChatRequest -> IO LLMTextResult) ->
  ChatStep ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))

-- | Generic non-streaming chat with a pluggable interpreter.
-- Use with @runStepServer store sid@ for persistence, or any custom interpreter.
generateTextWith ::
  (MonadIO m) =>
  ChatStepInterpreter m ->
  Maybe (GenerateText, WorkerMap) ->
  ChatEnv ->
  Conversation ->
  Text ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
generateTextWith interp mbGenWorkerMap unsafeEnv conv msg =
  let conv' = Conversation {unConversation = unConversation conv ++ [UserTurn msg]}
   in generateTextConversationWith interp mbGenWorkerMap unsafeEnv conv'

generateTextConversationWith ::
  (MonadIO m) =>
  ChatStepInterpreter m ->
  Maybe (GenerateText, WorkerMap) ->
  ChatEnv ->
  Conversation ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
generateTextConversationWith interp mbGenWorkerMap unsafeEnv conv = do
  let env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  liftIO $ onLog (envHooks env) Info $ "runChat: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv $ \mc c u ->
    let call = gwGenerateText (mcGateway mc) (envHooks env)
        step = buildChatStep mbGenWorkerMap env mc 0 u c
     in interp
          (envHooks env)
          (envAbortSignal env)
          (getFilteredToolsWithWorkers mbGenWorkerMap (envReadonly env) env)
          (envContextWindow env)
          (modelRetryPolicy mc)
          (mcRequestTimeout mc)
          call
          step

-- | Generic streaming chat with a pluggable interpreter.
streamTextWith ::
  (MonadIO m) =>
  ChatStepInterpreter m ->
  Maybe (GenerateText, WorkerMap) ->
  ChatEnv ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamTextWith interp mbGenWorkerMap unsafeEnv conv msg callback = do
  let conv' = Conversation {unConversation = unConversation conv ++ [UserTurn msg]}
   in streamTextConversationWith interp mbGenWorkerMap unsafeEnv conv' callback

streamTextConversationWith ::
  (MonadIO m) =>
  ChatStepInterpreter m ->
  Maybe (GenerateText, WorkerMap) ->
  ChatEnv ->
  Conversation ->
  (StreamEvent -> IO ()) ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamTextConversationWith interp mbGenWorkerMap unsafeEnv conv callback = do
  let env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  liftIO $ onLog (envHooks env) Info $ "streamChat: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv $ \mc c u ->
    let call req = liftIO $ gwStreamText (mcGateway mc) (envHooks env) req callback
        step = buildChatStep mbGenWorkerMap env mc 0 u c
     in interp
          (envHooks env)
          (envAbortSignal env)
          (getFilteredToolsWithWorkers mbGenWorkerMap (envReadonly env) env)
          (envContextWindow env)
          (modelRetryPolicy mc)
          (mcRequestTimeout mc)
          call
          step
