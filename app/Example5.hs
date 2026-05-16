module Example5 where

import Adapters.Repl (repl)
import LLM.Core.Logger (defaultDebugHooks)
import LLM.Core.Utils (toTool)
import LLM.Generate.Utils (addTool)
import LLM.Load.LoadEnvs (defaultEnvFilePaths, loadEnvsOrThrow)
import LLM.Tools.Worker (workerToolTyped)

main :: IO ()
main = do
  (env, workerEnv) <-
    loadEnvsOrThrow
      defaultEnvFilePaths
      defaultDebugHooks
      ("orchestrator", "worker")
  let env' =
        addTool
          ( toTool
              ( workerToolTyped
                  workerEnv
                  "worker"
                  "Worker tool that can execute arbitrary code"
              )
          )
          env
  repl env'
