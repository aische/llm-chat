module Tools.Weather (weatherTool) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import LLM.Types

weatherTool :: Tool
weatherTool =
  Tool
    { toolDef =
        ToolDef
          { toolName = "get_weather",
            toolDescription = "Get the current weather for a given location",
            toolParameters = weatherSchema
          },
      toolExecute = getWeather
    }

weatherSchema :: Value
weatherSchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "location"
              .= object
                [ "type" .= ("string" :: Text),
                  "description" .= ("City name, e.g. London" :: Text)
                ]
          ],
      "required" .= (["location"] :: [Text])
    ]

-- | Dummy implementation — in reality you'd call a weather API
getWeather :: Value -> IO Text
getWeather args = do
  let loc = fromMaybe "unknown" $ parseMaybe parseLocation args
  pure $ "Weather in " <> loc <> ": Partly cloudy, 18°C, light breeze from the west."

parseLocation :: Value -> Parser Text
parseLocation = withObject "args" (.: "location")
