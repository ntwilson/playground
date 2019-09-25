module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Prim.Row (class Union)

--------------------------------------------------------------------------------------
-- METHOD 0
-- Define a type (well, a type class) with Required fields and the entire set of required + optional
-- fields that you could use from many functions.  This is one of the most complicated method of optional 
-- fields.
--------------------------------------------------------------------------------------

type Required = (b :: Int)
type AllFields = (a :: Int, b :: Int)

class Method0Rec (given :: #Type) (rest :: #Type) (optionalGiven :: #Type) 
instance method0Rec :: 
  ( Union given rest AllFields -- given must be a subset of AllFields (given + rest = AllFields)
  , Union Required optionalGiven given -- given must be a superset of Required (Required + any optional given = given)
  ) => Method0Rec given rest optionalGiven

foreign import frgn :: 
  forall given rest optionalGiven. 
  Method0Rec given rest optionalGiven => 
  (Record given) -> Int

x :: Int 
x = frgn { b: 5 }

y :: Int
y = frgn { a: 5, b: 3 }

-- should not compile because `b` is required but not given
-- z :: Int
-- z = frgn { a: 3 }

-- should not compile because `z` is given but not required or optional
-- z :: Int
-- z = frgn { b: 3, z: 5 }

--------------------------------------------------------------------------------------
-- METHOD 1
-- Define a type (well, a type class) with Required and Optional fields that you 
-- could use from many functions.  This is the most complicated method of optional 
-- fields, more complicated than method 0 in that it adds a type parameter and an 
-- extra constraint on the type class instance, but has the advantage that you do
-- not need to duplicate the required fields in a Required type binding _and_ in 
-- an AllFields type binding.
--------------------------------------------------------------------------------------

-- references Required from above, copied here for convenience
-- type Required = (b :: Int)
type Optional = (a :: Int)

class Method1Rec (allFields :: #Type) (given :: #Type) (rest :: #Type) (optionalGiven :: #Type) 
instance method1Rec :: 
  ( Union Required Optional allFields -- allFields must be the union of required fields and optional fields
  , Union given rest allFields -- given must be a subset of AllFields (given + rest = AllFields)
  , Union Required optionalGiven given -- given must be a superset of Required (Required + any optional given = given)
  ) => Method1Rec allFields given rest optionalGiven

foreign import frgn1 :: 
  forall allFields given rest optionalGiven. 
  Method1Rec allFields given rest optionalGiven => 
  (Record given) -> Int

x1 :: Int 
x1 = frgn1 { b: 5 }

y1 :: Int
y1 = frgn1 { a: 5, b: 3 }

-- should not compile because `b` is required but not given
-- z :: Int
-- z = frgn { a: 3 }

-- should not compile because `z` is given but not required or optional
-- z :: Int
-- z = frgn { b: 3, z: 5 }


--------------------------------------------------------------------------------------
-- METHOD 2
-- Define a type (well, a type class) with Optional fields that you 
-- could use from many functions.  This can be used when _all_ fields have defaults,
-- and it would be valid to pass in a completely empty record.  This is a much simpler
-- solution than method 1 for cases where you don't have required fields.
--------------------------------------------------------------------------------------

-- references AllFields from above, copied here for convenience
-- type AllFields = (a :: Int, b :: Int)

class Method2Rec (given :: #Type) (rest :: #Type)
instance method2Rec :: 
  (Union given rest AllFields -- given must be a subset of AllFields (given + rest = AllFields)
  ) => Method2Rec given rest

foreign import frgn2 :: 
  forall given rest.
  Method2Rec given rest =>
  (Record given) -> Int 

x2 :: Int
x2 = frgn2 {}

y2 :: Int
y2 = frgn2 { a: 5 }

z2 :: Int
z2 = frgn2 { b: 5 }

aa2 :: Int
aa2 = frgn2 { a: 5, b: 3 }

-- should not compile because `z` is given but not allowed in AllFields
-- ab2 :: Int
-- ab2 = frgn2 { a: 5, z: 2 }

--------------------------------------------------------------------------------------
-- METHOD 3
-- Define a type (well, a type class) for optional fields, and a function that includes
-- required fields.  This is a much simpler approach than method 1 for cases where the
-- required fields are only needed by a single function, or it is simple to redefine
-- the required fields for each function with that constraint.
--------------------------------------------------------------------------------------

-- references Optional from above, copied here for convenience
-- type Optional = (a :: Int) 

class Method3Rec (optionalGiven :: #Type) (rest :: #Type)
instance method3Rec ::
  ( Union optionalGiven rest Optional  -- optionalGiven must be a subset of Optional (optionalGiven + rest = Optional)
  ) => Method3Rec optionalGiven rest

foreign import frgn3 ::
  forall optionalGiven rest.
  Method3Rec optionalGiven rest =>
  { b :: Int | optionalGiven } -> Int

x3 :: Int 
x3 = frgn3 { b: 5 }

y3 :: Int
y3 = frgn3 { a: 5, b: 3 }

-- should not compile because `b` is required but not given
-- z :: Int
-- z = frgn3 { a: 3 }

-- should not compile because `z` is given but not required or optional
-- z :: Int
-- z = frgn3 { b: 3, z: 5 }
  

main :: Effect Unit
main = do
  logShow y
