module Example5 where

import Adapters.Repl (repl)
import LLM.Core.Logger (defaultDebugHooks)
import LLM.Core.Utils (toTool)
import LLM.Generate.Generate (generateText)
import LLM.Generate.Utils (addTool)
import LLM.Generate.WorkerTool (workerToolTyped)
import LLM.Load.LoadEnvs (defaultEnvFilePaths, loadEnvsOrThrow)

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
                  generateText
                  workerEnv
                  "worker"
                  "Worker tool that can execute arbitrary code"
              )
          )
          env
  repl env'
