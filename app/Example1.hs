module Example1 where

import Data.Text.IO qualified as TIO
import LLM (createChatEnv, createModelConfig, emptyConversation, ollamaGateway)
import LLM.Generate.Generate (generateText)

main :: IO ()
main = do
  let mc = createModelConfig ollamaGateway "llama3.2"
      env = createChatEnv mc "" []

  result <- generateText env emptyConversation "what is the capital of france?"
  case result of
    Left e -> print e
    Right (answer, _, _) -> TIO.putStrLn answer
