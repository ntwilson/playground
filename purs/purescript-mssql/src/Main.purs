module Main where

import Prelude

import Control.Monad.Except (Except, ExceptT(..), except, runExcept, runExceptT, withExcept, withExceptT)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Database (connect, query)
import Database.ORM (KeyedValues, KeyedValuesJson, WeatherValues, WeatherValuesJson)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, message)
import Effect.Class.Console (log)
import Foreign.Generic (class Decode, Foreign, decode)

decode' :: forall m a. Monad m => Decode a => Foreign -> ExceptT String m a
decode' = decode >>> withExcept show >>> switchMonad

switchMonad :: forall e m a. Monad m => Except e a -> ExceptT e m a 
switchMonad = runExcept >>> except


getKeyedValues :: Aff (Either String (Array KeyedValues))
getKeyedValues = runExceptT $ do
  conn <- connect "mssql03.ad.mea.energy" "SystemTestSource" # ExceptT # withExceptT message
  result <- query conn "SELECT * FROM KeyedValues" # ExceptT # withExceptT message
  (keyedValues :: Array KeyedValuesJson) <- traverse decode' result 
  pure (unwrap <$> keyedValues)


getAFewWeatherValues :: Aff (Either String (Array WeatherValues)) 
getAFewWeatherValues = runExceptT $ do
  conn <- connect "mssql03.ad.mea.energy" "SystemTestSource" # ExceptT # withExceptT message
  results <- query conn "SELECT TOP 10 * FROM weather_values" # ExceptT # withExceptT message
  (keyedValues :: Array WeatherValuesJson) <- traverse decode' results
  pure (unwrap <$> keyedValues)


main :: Effect Unit
main = launchAff_ do
  result <- getKeyedValues
  case result of 
    Right values -> 
      log ("Got 'em! " <> show values)
    Left error ->
      log ("uh oh. " <> error)

