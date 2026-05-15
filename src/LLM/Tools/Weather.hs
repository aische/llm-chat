module LLM.Tools.Weather (weatherToolTyped) where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text, toLower)
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))

newtype WeatherToolArgs = WeatherToolArgs
  { _weatherLocation :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec WeatherToolArgs)

instance AC.HasCodec WeatherToolArgs where
  codec =
    AC.object "WeatherToolArgs" $
      WeatherToolArgs
        <$> AC.requiredField "location" "City name, e.g. London" AC..= _weatherLocation

weatherToolTyped :: TypedTool WeatherToolArgs
weatherToolTyped =
  TypedTool
    { ttoolName = "get_weather",
      ttoolDescription = "Get the current weather for a given location",
      ttoolReadonly = False,
      ttoolExecute = const getWeather
    }

-- | Dummy implementation — in reality you'd call a weather API
getWeather :: WeatherToolArgs -> IO Text
getWeather args = do
  let loc = _weatherLocation args
  case toLower loc of
    "london" -> pure "Weather in London is partly cloudy, 18°C, light breeze from the west."
    "paris" -> pure "Weather in Paris is sunny, 23°C, no wind."
    _ -> pure $ "Weather in" <> loc <> " is rainy, 12°C, strong wind from the east."
