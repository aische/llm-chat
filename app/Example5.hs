module Example5 where

import Adapters.Repl (repl)
import LLM (addTool, defaultDebugHooks, toTool)
import LLM.Generate.LoadModels (loadEnvsOrThrow)
import LLM.Tools.Worker (workerToolTyped)

main :: IO ()
main = do
  (env, workerEnv) <- loadEnvsOrThrow defaultDebugHooks ("orchestrator", "worker")
  let env' = addTool (toTool (workerToolTyped workerEnv "worker")) env
  repl env'
