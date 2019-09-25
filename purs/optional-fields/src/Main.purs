module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Prim.Row (class Union)

type Required = (b :: Int)
type AllFields = (a :: Int, b :: Int)

class InputRec (given :: #Type) (rest :: #Type) (optionalGiven :: #Type)
instance inputRec :: 
  ( Union given rest AllFields -- given must be a subset of AllFields (given + rest = AllFields)
  , Union Required optionalGiven given -- given must be a superset of Required (Required + any optional given = given)
  ) => InputRec given rest optionalGiven

foreign import frgn :: 
  forall given rest optionalGiven. 
  InputRec given rest optionalGiven => 
  (Record given) -> Int

x :: Int 
x = frgn { b: 5 }

y :: Int
y = frgn { a: 5, b: 3 }

-- z :: Int
-- z = frgn { a: 3 }

main :: Effect Unit
main = do
  logShow y
