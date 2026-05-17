module LLM.Tools.Age (ageTool) where

import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Aeson
  ( KeyValue ((.=)),
    Value,
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text, toLower)
import LLM.Core.Types
  ( Tool (..),
    ToolDef (..),
  )

ageTool :: (MonadUnliftIO m) => Tool m
ageTool =
  Tool
    { toolDef =
        ToolDef
          { toolName = "get_age",
            toolDescription = "Get the age of a given person",
            toolReadonly = True,
            toolParameters = ageSchema
          },
      toolExecute = const getAge
    }

ageSchema :: Value
ageSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "person"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("Name of the person whose age is being requested" :: Text)
                ]
          ],
      "required" .= (["person"] :: [Text])
    ]

-- | Dummy implementation — in reality you'd call a weather API
getAge :: (MonadUnliftIO m) => Value -> m Text
getAge args = liftIO $ do
  let name = fromMaybe "unknown" $ parseMaybe parsePersonName args
  -- error "Age database is currently unavailable"
  if toLower name == "alice"
    then pure "Alice is 30 years old."
    else
      if toLower name == "bob"
        then pure "Bob is 25 years old."
        else pure $ name <> " is 41 years old."

parsePersonName :: Value -> Parser Text
parsePersonName = withObject "args" (.: "person")
