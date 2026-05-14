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
import LLM.Generate.Types
  ( ChatEnv (..),
    ModelConfig (..),
  )
import LLM.Generate.Utils (modelRetryPolicy)
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
  ChatEnv ->
  Conversation ->
  Text ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
generateTextWith interp unsafeEnv conv msg =
  let conv' = Conversation {unConversation = unConversation conv ++ [UserTurn msg]}
   in generateTextConversationWith interp unsafeEnv conv'

generateTextConversationWith ::
  (MonadIO m) =>
  ChatStepInterpreter m ->
  ChatEnv ->
  Conversation ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
generateTextConversationWith interp unsafeEnv conv = do
  let env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  liftIO $ onLog (envHooks env) Info $ "runChat: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv $ \mc c u ->
    let call = gwGenerateText (mcGateway mc) (envHooks env)
        step = buildChatStep env mc 0 u c
     in interp
          (envHooks env)
          (envAbortSignal env)
          (envTools env)
          (envContextWindow env)
          (modelRetryPolicy mc)
          (mcRequestTimeout mc)
          call
          step

-- | Generic streaming chat with a pluggable interpreter.
streamTextWith ::
  (MonadIO m) =>
  ChatStepInterpreter m ->
  ChatEnv ->
  Conversation ->
  Text ->
  (StreamEvent -> IO ()) ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamTextWith interp unsafeEnv conv msg callback = do
  let conv' = Conversation {unConversation = unConversation conv ++ [UserTurn msg]}
   in streamTextConversationWith interp unsafeEnv conv' callback

streamTextConversationWith ::
  (MonadIO m) =>
  ChatStepInterpreter m ->
  ChatEnv ->
  Conversation ->
  (StreamEvent -> IO ()) ->
  m (Either (LLMError, Conversation, Usage) (Text, Conversation, Usage))
streamTextConversationWith interp unsafeEnv conv callback = do
  let env = unsafeEnv {envHooks = safeHooks (envHooks unsafeEnv)}
  liftIO $ onLog (envHooks env) Info $ "streamChat: tools=" <> T.pack (show (length (envTools env)))
  withFallback env conv $ \mc c u ->
    let call req = liftIO $ gwStreamText (mcGateway mc) (envHooks env) req callback
        step = buildChatStep env mc 0 u c
     in interp
          (envHooks env)
          (envAbortSignal env)
          (envTools env)
          (envContextWindow env)
          (modelRetryPolicy mc)
          (mcRequestTimeout mc)
          call
          step
