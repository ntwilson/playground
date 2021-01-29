module Database where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)


foreign import data Connection :: Type
foreign import connectImpl :: EffectFn2 String String (Promise Connection)
foreign import queryImpl :: EffectFn2 Connection String (Promise (Array Foreign))
foreign import closeImpl :: EffectFn1 Connection (Promise Unit)

connect :: String -> String -> Aff Connection 
connect server database = 
  toAffE (runEffectFn2 connectImpl server database)

query :: Connection -> String -> Aff (Array Foreign)
query conn queryStr = 
  toAffE (runEffectFn2 queryImpl conn queryStr)

close :: Connection -> Aff Unit
close conn = 
  toAffE (runEffectFn1 closeImpl conn)

