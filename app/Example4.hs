module Example4 where

import LLM.Core.Logger
    ( LogLevel(Debug), noHooks, withJsonDump, withStderrLogger )
import LLM.Generate.Generate ( generateText )
import LLM.Core.Utils (emptyConversation)
import LLM.Load.LoadEnvs (defaultEnvFilePaths, loadEnvsOrThrow)

main :: IO ()
main = do
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
  (funnyEnv, angryEnv) <-
    loadEnvsOrThrow
      defaultEnvFilePaths
      hooks
      ("funny", "angry")
  r <- generateText funnyEnv emptyConversation "what is the capital of france?"
  print r
  r2 <- generateText angryEnv emptyConversation "what is the capital of france?"
  print r2
