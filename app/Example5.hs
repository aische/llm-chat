module Example5 where

import Adapters.Repl (repl)
import LLM.Core.Logger (defaultDebugHooks)
import LLM.Load.LoadEnvs (defaultEnvFilePaths, loadEnvOrThrow)
import LLM.Load.Types (LoadedEnvs (workerMap))

main :: IO ()
main = do
  (env, envs) <-
    loadEnvOrThrow
      defaultEnvFilePaths
      "orchestrator"
      defaultDebugHooks
  repl (workerMap envs) env
