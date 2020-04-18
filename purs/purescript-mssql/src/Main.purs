module Main where

import Prelude

import Control.Monad.Except (ExceptT(..), except, runExcept, runExceptT, withExcept, withExceptT)
import Control.Promise (Promise, toAffE)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.JSDate (JSDate, readDate)
import Data.JSDate as JSDate
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff_, message)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Generic (class Decode, Foreign, decode)
import Partial.Unsafe (unsafePartial)

foreign import data Connection :: Type
foreign import connectImpl :: EffectFn2 String String (Promise Connection)
foreign import queryImpl :: EffectFn2 Connection String (Promise (Array Foreign))

connect :: String -> String -> Aff (Either Error Connection) 
connect server database = 
  attempt (toAffE (runEffectFn2 connectImpl server database))

query :: Connection -> String -> Aff (Either Error (Array Foreign))
query conn queryStr = 
  attempt (toAffE (runEffectFn2 queryImpl conn queryStr))

type KeyedValues = { keyName :: String, value :: String }
newtype KeyedValuesJson = KeyedValuesJson KeyedValues
derive instance newtypeKeyedValues :: Newtype KeyedValuesJson _
instance decodeKeyedValues :: Decode KeyedValuesJson where
  decode json = 
    case runExcept $ decode json of
      Right ({ "KeyName": keyName, "Value": value } :: { "KeyName"::_, "Value"::_ }) -> 
        pure $ KeyedValuesJson { keyName, value }
      Left err -> except $ Left err

getKeyedValues :: Aff (Either String (Array KeyedValues))
getKeyedValues = runExceptT $ do
  conn <- ExceptT $ (connect "mssql03.ad.mea.energy" "SystemTestSource" <#> lmap message)
  result <- ExceptT $ (query conn "SELECT * FROM KeyedValues" <#> lmap message)
  (keyedValues :: Array KeyedValuesJson) <- except (runExcept (traverse decode result # withExcept show))
  pure (unwrap <$> keyedValues)

newtype JSDateDecoder = JSDateDecoder JSDate
instance decodeJSDate :: Decode JSDateDecoder where 
  decode json = 
    JSDateDecoder <$> readDate json 

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
newtype WeatherValuesJson = WeatherValuesJson WeatherValues
derive instance newtypeWeatherValues :: Newtype WeatherValuesJson _
instance decodeWeatherValues :: Decode WeatherValuesJson where
  decode json = case runExcept $ decode json of
    Right 
      ({ time, weather_value, weather_attribute, weather_station, file_timestamp, entry_timestamp, latest_timestamp, is_forecasted } 
        :: { time::_, weather_value::_, weather_attribute::_, weather_station::_, file_timestamp::_, entry_timestamp::_, latest_timestamp::_, is_forecasted::_ }) ->

      let 
        (JSDateDecoder time) = time
        (JSDateDecoder fileTimestamp) = file_timestamp
        (JSDateDecoder entryTimestamp) = entry_timestamp
        (JSDateDecoder latestTimestamp) = latest_timestamp


      in pure $ WeatherValuesJson 
        { time: unsafePartial $ fromJust $ JSDate.toInstant time
        , weatherValue: weather_value
        , weatherAttribute: weather_attribute
        , weatherStation: weather_station
        , fileTimestamp: JSDate.toInstant fileTimestamp
        , entryTimestamp: unsafePartial $ fromJust $ JSDate.toInstant entryTimestamp
        , latestTimestamp: unsafePartial $ fromJust $ JSDate.toInstant latestTimestamp
        , isForecasted: is_forecasted
        }
    Left err -> except $ Left err

getAFewWeatherValues :: Aff (Either String (Array WeatherValues)) 
getAFewWeatherValues = runExceptT $ do
  conn <- ExceptT $ (connect "mssql03.ad.mea.energy" "SystemTestSource" <#> lmap message)
  result <- ExceptT $ (query conn "SELECT TOP 10 * FROM weather_values" <#> lmap message)
  (keyedValues :: Array WeatherValuesJson) <- except (runExcept (traverse decode result # withExceptT show))
  pure (unwrap <$> keyedValues)


main :: Effect Unit
main = launchAff_ do
  result <- getAFewWeatherValues
  liftEffect $ case result of 
    Right values -> 
      log ("Got 'em! " <> show values)
    Left error ->
      log ("uh oh. " <> error)

