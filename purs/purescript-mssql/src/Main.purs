module Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Database (Connection, close, connect, query)
import Database.ORM (KeyedValues, KeyedValuesDecoder, WeatherValues, WeatherValuesDecoder)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error, error, message)
import Foreign (ForeignError)
import Foreign.Generic (class Decode, Foreign, decode)

decode' :: forall a. Decode a => Foreign -> Either (NonEmptyList ForeignError) a
decode' = decode >>> runExceptT >>> unwrap

eitherToM :: forall m e a. MonadThrow Error m => Show e => Either e a -> m a
eitherToM (Left e) = throwError (error (show e)) 
eitherToM (Right a) = pure a


getKeyedValues :: Connection -> Aff (Array KeyedValues)
getKeyedValues conn = do
  result <- query conn "SELECT * FROM KeyedValues" 
  (keyedValues :: Array KeyedValuesDecoder) <- traverse (decode' >>> eitherToM) result 
  pure (unwrap <$> keyedValues)


getAFewWeatherValues :: Connection -> Aff (Array WeatherValues)
getAFewWeatherValues conn = do
  results <- query conn "SELECT TOP 10 * FROM weather_values" 
  (keyedValues :: Array WeatherValuesDecoder) <- traverse (decode' >>> eitherToM) results
  pure (unwrap <$> keyedValues)


main :: Effect Unit
main = launchAff_ do
  
  conn <- connect "mssql03.ad.mea.energy" "SystemTestSource" 
  keyedValues <- attempt (getKeyedValues conn)
  showResults "KeyedValues" keyedValues

  wxValues <- attempt (getAFewWeatherValues conn)
  showResults "WeatherValues" wxValues

  _ <- close conn
  pure unit


  where
    showResults :: forall r m. Show r => MonadEffect m => String -> Either Error (Array r) -> m Unit
    showResults name results = 
      case results of
        Right values -> log ("Got " <> name <> "! " <> show values) 
        Left error -> log ("uh oh. " <> message error)


