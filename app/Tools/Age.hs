module Tools.Age (ageTool) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Text (Text)
import LLM.Types

ageTool :: Tool
ageTool =
  Tool
    { toolDef =
        ToolDef
          { toolName = "get_age",
            toolDescription = "Get the age of a given person",
            toolParameters = ageSchema
          },
      toolExecute = getAge
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
getAge :: Value -> IO Text
getAge args = do
  let name = maybe "unknown" id $ parseMaybe parsePersonName args
  -- error "Age database is currently unavailable"
  pure $ "Age of " <> name <> ":41 years old."

parsePersonName :: Value -> Parser Text
parsePersonName = withObject "args" (.: "person")
