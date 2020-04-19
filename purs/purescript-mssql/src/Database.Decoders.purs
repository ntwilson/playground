module Database.Decoders where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.JSDate (JSDate, readDate)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..), fail, readNullOrUndefined)
import Foreign.Generic (class Decode, decode)


newtype JSDateDecoder = JSDateDecoder JSDate
instance decodeJSDate :: Decode JSDateDecoder where 
  decode json = 
    JSDateDecoder <$> readDate json 

newtype InstantFromDB = InstantFromDB Instant
derive instance newtypeInstant :: Newtype InstantFromDB _ 
instance decodeInstant :: Decode InstantFromDB where
  decode x = 
    case runExcept $ decode x of
      Right (JSDateDecoder date) | Just inst <- JSDate.toInstant date -> 
        pure $ InstantFromDB inst
      Right (JSDateDecoder date) | otherwise -> 
        fail $ ForeignError ("couldn't convert JSDate " <> show date <> " to an Instant.")
      Left err -> throwError err

newtype DBNull a = DBNull (Maybe a)
derive instance newtypeDBNull :: Newtype (DBNull a) _
instance decodeDBNull :: Decode a => Decode (DBNull a) where
  decode x = do
    a <- readNullOrUndefined x
    case a of
      Just b -> (DBNull <<< Just) <$> decode b
      Nothing -> pure $ DBNull Nothing

