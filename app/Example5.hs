module Example5 where

import Adapters.Repl (repl)
import LLM (addTool, defaultDebugHooks, toTool)
import LLM.Load.LoadEnvs (defaultEnvFilePaths, loadEnvsOrThrow)
import LLM.Tools.Worker (workerToolTyped)

main :: IO ()
main = do
  (env, workerEnv) <-
    loadEnvsOrThrow
      defaultEnvFilePaths
      defaultDebugHooks
      ("orchestrator", "worker")
  let env' = addTool (toTool (workerToolTyped workerEnv "worker")) env
  repl env'
