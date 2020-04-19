module Database.ORM where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Database.Decoders (DBNull(..), InstantFromDB(..))
import Foreign.Generic (class Decode, decode)


type KeyedValues = { keyName :: String, value :: String }
newtype KeyedValuesDecoder = KeyedValuesDecoder KeyedValues
derive instance newtypeKeyedValues :: Newtype KeyedValuesDecoder _
instance decodeKeyedValues :: Decode KeyedValuesDecoder where
  decode x = 
    case runExcept $ decode x of
      Right ({ "KeyName": keyName, "Value": value } :: { "KeyName"::_, "Value"::_ }) -> 
        pure $ KeyedValuesDecoder { keyName, value }
      Left err -> throwError err
      

type WeatherValues = 
  { time :: Instant
  , weatherValue :: Number
  , weatherAttribute :: Int
  , weatherStation :: Int
  , fileTimestamp :: Maybe Instant
  , entryTimestamp :: Instant
  , latestTimestamp :: Instant
  , isForecasted :: Boolean
  }
newtype WeatherValuesDecoder = WeatherValuesDecoder WeatherValues
derive instance newtypeWeatherValues :: Newtype WeatherValuesDecoder _
instance decodeWeatherValues :: Decode WeatherValuesDecoder where
  decode x = case runExcept $ decode x of
    Right 
      ( { time: (InstantFromDB time)
        , weather_value
        , weather_attribute
        , weather_station
        , file_timestamp: (DBNull (maybeFileTimestamp :: Maybe InstantFromDB))
        , entry_timestamp: (InstantFromDB entryTimestamp)
        , latest_timestamp: (InstantFromDB latestTimestamp)
        , is_forecasted 
        } :: { time::_, weather_value::_, weather_attribute::_, weather_station::_, file_timestamp::_, entry_timestamp::_, latest_timestamp::_, is_forecasted::_ }) ->

      pure $ WeatherValuesDecoder 
        { time
        , weatherValue: weather_value
        , weatherAttribute: weather_attribute
        , weatherStation: weather_station
        , fileTimestamp: maybeFileTimestamp <#> unwrap
        , entryTimestamp: entryTimestamp
        , latestTimestamp: latestTimestamp
        , isForecasted: is_forecasted
        }
    Left err -> throwError err

