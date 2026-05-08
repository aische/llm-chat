{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import AllModels (AllModels (..), getAllModels)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, catch)
import Example (example)
import Main1 (main1)
import TestExample (testExample)

-- main :: IO ()
-- main = main1

main :: IO ()
main = do
  loadFile defaultConfig `catch` \(_ :: SomeException) -> pure ()
  AllModels {gemini_2_5_flash, claude_haiku_4_5, llama_3_2, gpt_4_1, gpt_5_nano} <- getAllModels
  testExample False llama_3_2
