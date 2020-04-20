module Main where

import Prelude

import Control.Monad.Except (Except, ExceptT(..), except, runExcept, runExceptT, withExcept, withExceptT)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Database (Connection, close, connect, query)
import Database.ORM (KeyedValues, KeyedValuesDecoder, WeatherValues, WeatherValuesDecoder)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, message)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign.Generic (class Decode, Foreign, decode)

decode' :: forall m a. Monad m => Decode a => Foreign -> ExceptT String m a
decode' = decode >>> withExcept show >>> switchMonad

switchMonad :: forall e m a. Monad m => Except e a -> ExceptT e m a 
switchMonad = runExcept >>> except


getKeyedValues :: Connection -> Aff (Either String (Array KeyedValues))
getKeyedValues conn = runExceptT $ do
  result <- query conn "SELECT * FROM KeyedValues" # ExceptT # withExceptT message
  (keyedValues :: Array KeyedValuesDecoder) <- traverse decode' result 
  pure (unwrap <$> keyedValues)


getAFewWeatherValues :: Connection -> Aff (Either String (Array WeatherValues)) 
getAFewWeatherValues conn = runExceptT $ do
  results <- query conn "SELECT TOP 10 * FROM weather_values" # ExceptT # withExceptT message
  (keyedValues :: Array WeatherValuesDecoder) <- traverse decode' results
  pure (unwrap <$> keyedValues)


main :: Effect Unit
main = launchAff_ do
  
  tryConn <- connect "mssql03.ad.mea.energy" "SystemTestSource" 
  case tryConn of 
    Right conn -> do
      keyedValues <- getKeyedValues conn
      showResults "KeyedValues" keyedValues

      wxValues <- getAFewWeatherValues conn
      showResults "WeatherValues" wxValues

      _ <- close conn
      pure unit

    Left error -> 
      log ("Could not even connect to the database. " <> message error)

  where
    showResults :: forall r m. Show r => MonadEffect m => String -> Either String (Array r) -> m Unit
    showResults name results = 
      case results of
        Right values -> log ("Got " <> name <> "! " <> show values) 
        Left error -> log ("uh oh. " <> error)


