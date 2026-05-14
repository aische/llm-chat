module Example4 where

import LLM (LogLevel (Debug), generateText, noHooks, withJsonDump, withStderrLogger)
import LLM.Core.Utils (emptyConversation)
import LLM.Generate.LoadModels (loadEnvsOrThrow)

main :: IO ()
main = do
  let hooks = withJsonDump "./dumps" . withStderrLogger Debug $ noHooks
  (funnyEnv, angryEnv) <- loadEnvsOrThrow hooks ("funny", "angry")
  r <- generateText funnyEnv emptyConversation "what is the capital of france?"
  print r
  r2 <- generateText angryEnv emptyConversation "what is the capital of france?"
  print r2
