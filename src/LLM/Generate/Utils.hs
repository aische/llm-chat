module LLM.Generate.Utils
  ( defaultChatEnv,
    createChatEnv,
    createModelConfig,
    addTool,
    getToolsWithWorkers,
    addHooksToWorkerMap,
  )
where

import Data.Map qualified as Map
import Data.Text (Text)
import LLM.Core.Logger (Hooks, noHooks)
import LLM.Core.Types (LLMGateway, Tool)
import LLM.Core.Usage (PricingInfo (..))
import LLM.Core.Utils (toTool)
import LLM.Generate.Types (ChatEnv (..), GenerateText, ModelConfig (..), Worker (..), WorkerMap)
import LLM.Generate.WorkerTool (workerToolTyped)

-- | Sensible defaults — single model, no fallback.
defaultChatEnv :: ModelConfig -> ChatEnv
defaultChatEnv mc =
  ChatEnv
    { envModel = mc,
      envFallbacks = [],
      envSystem = Nothing,
      envTools = [],
      envReadonly = False,
      envMaxToolRounds = 10,
      envContextWindow = Nothing,
      envHooks = noHooks,
      envWorkers = Nothing,
      envAbortSignal = Nothing
    }

createChatEnv :: ModelConfig -> Text -> [Tool] -> ChatEnv
createChatEnv mc system tools =
  ChatEnv
    { envModel = mc,
      envFallbacks = [],
      envSystem = Just system,
      envTools = tools,
      envReadonly = False,
      envMaxToolRounds = 10,
      envContextWindow = Nothing,
      envHooks = noHooks,
      envWorkers = Nothing,
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
      mcRetryCount = 0,
      mcJitterBackoff = 1_000
    }

addTool :: Tool -> ChatEnv -> ChatEnv
addTool tool env = env {envTools = tool : envTools env}

getToolsWithWorkers :: Maybe (GenerateText, WorkerMap) -> ChatEnv -> [Tool]
getToolsWithWorkers Nothing chatEnv = envTools chatEnv
getToolsWithWorkers (Just (gen, workerMap)) chatEnv =
  let workerTools =
        case envWorkers chatEnv of
          Nothing -> []
          Just workerNames ->
            flip map workerNames $ \wname ->
              case Map.lookup wname workerMap of
                Nothing -> error ""
                Just worker ->
                  let name = workerName worker
                      desc = workerDescription worker
                      env = workerEnv worker
                   in toTool (workerToolTyped gen env name desc)
   in envTools chatEnv ++ workerTools

addHooksToWorkerMap :: Hooks -> Maybe WorkerMap -> Maybe WorkerMap
addHooksToWorkerMap _hooks Nothing = Nothing
addHooksToWorkerMap hooks (Just workerMap) =
  Just $
    Map.map
      (\v -> v {workerEnv = (workerEnv v) {envHooks = hooks}})
      workerMap
