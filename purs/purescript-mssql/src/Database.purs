module Database where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Either (Either)
import Effect.Aff (Aff, Error, attempt)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)


foreign import data Connection :: Type
foreign import connectImpl :: EffectFn2 String String (Promise Connection)
foreign import queryImpl :: EffectFn2 Connection String (Promise (Array Foreign))
foreign import closeImpl :: EffectFn1 Connection (Promise Unit)

connect :: String -> String -> Aff (Either Error Connection) 
connect server database = 
  attempt (toAffE (runEffectFn2 connectImpl server database))

query :: Connection -> String -> Aff (Either Error (Array Foreign))
query conn queryStr = 
  attempt (toAffE (runEffectFn2 queryImpl conn queryStr))

close :: Connection -> Aff (Either Error Unit)
close conn = 
  attempt (toAffE (runEffectFn1 closeImpl conn))

