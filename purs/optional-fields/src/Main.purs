module Main where

import Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

-- This codebase documents several approaches for dealing with JS FFI where JS
-- expects an object with optional properties.  
-- 
-- Methods 0 - 3 show how to actually encode records with optional properties in the 
-- Purescript type system.  These approaches are the safest and most convenient to consume
-- but involve some type class constraint sorcery that can be more challenging to understand
-- than the other methods.
-- Methods 4 & 5 are much simpler, but have drawbacks.  Method 4 is more difficult to consume
-- and will ultimately set all optional fields to undefined (which in JS is subtly different than
-- if the field isn't there at all).  Method 5 simply abandons any typechecking of the optional
-- properties, and allows the caller to pass in whatever additional properties they desire.
-- 
-- Method 0: Supports required and optional fields, but requires some duplication in that 
-- you must define the required fields in one type definition, and also all required + optional
-- fields in another type definition
-- 
-- Method 1: Supports required and optional fields, only requiring separate type definitions
-- for just the required fields and another for just the optional fields, but the type class
-- sorcery is even more complex than method 0
-- 
-- Method 2: Does not support required fields.  The complexity goes down significantly, but
-- is only usable when no fields are required and an empty object is a valid input.
-- 
-- Method 3: Defines the type with the same technique as method 2, but defines required 
-- fields as part of the function definition.  This is the least complex method of supporting
-- required and optional fields, but if more than one function work with the same type, the 
-- required fields would need to be repeated for each function definition.
-- 
-- Method 4: A different approach to optional properties, where they are treated as not optional
-- as far as the purescript compiler is concerned, and we use native (and simple) purescript strategies
-- to set the defaults for each optional property manually to `undefined`. (Note that this would 
-- produce a different behavior if from the JS code, just the presense of the field, even if it is 
-- undefined, is treated differently than if the field wasn't there.  This can be worked around with
-- additional complexity.  An example of that is here: https://blog.ndk.io/purescript-ffi.html) 
-- 
-- Method 5: The simplest approach, but not as safe as the others. Only defines required fields 
-- (if there are any), but allowing the caller to pass in any additional fields they want to.
-- This allows for any optional fields to be passed in, but the typechecker only checks to 
-- make sure the required fields are present.  It doesn't ensure that any other fields are 
-- recognized



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


--------------------------------------------------------------------------------------
-- METHOD 4
-- Define a type that includes optional fields as Maybes, and a constructor function
-- that defaults each optional field to Nothing.  
-- This is a much simpler approach than any of the above methods, but requires more
-- boilerplate, and could introduce subtle bugs if the JS code being called treats 
-- an existing field that is undefined differently than a field that is not present.
-- Another advantage to this approach is that the optional fields can be consumed by
-- PureScript code where it isn't known by the compiler if the optional value is present
-- or not.
--------------------------------------------------------------------------------------

-- this stuff at the top is for working with JS undefined.  It would only need to be defined
-- once, not for each type that has optional fields
foreign import data Undefined :: Type -> Type
foreign import undefined :: forall a. Undefined a

maybeToUndefined :: forall a. Maybe a -> Undefined a
maybeToUndefined (Just v) = unsafeCoerce v
maybeToUndefined Nothing = undefined

foreign import frgn4Impl :: { a :: Undefined Int, b :: Int } -> Int

type OptionalFields = { a :: Maybe Int, b :: Int }
frgn4 :: { b :: Int } -> (OptionalFields -> OptionalFields) -> Int
frgn4 { b } copyAndUpdate =
  let 
    {a, b} =
      { b, a: Nothing } 
      # copyAndUpdate

  in frgn4Impl { a: maybeToUndefined a, b }

x4 :: Int 
x4 = frgn4 { b: 5 } identity

y4 :: Int
y4 = frgn4 { b: 3 } (_ { a = Just 5 })  



--------------------------------------------------------------------------------------
-- METHOD 5
-- Define the foreign function by only providing the required fields.  The typechecker 
-- simply ignores any optional fields.  This means that if you pass in a field that
-- isn't recognized by the JS code, it will just be ignored and there will be no 
-- compile error.  
-- This is the absolute simplest approach, at the cost of some typechecking that you
-- get with the other approaches.
--------------------------------------------------------------------------------------

foreign import frgn5 :: forall r. { b :: Int | r } -> Int

x5 :: Int 
x5 = frgn5 { b: 5 }

y5 :: Int
y5 = frgn5 { a: 5, b: 3 }

-- compiles even though `z` is not recognized
z5 :: Int
z5 = frgn5 { b: 3, z: 5 }

-- should not compile because `b` is required but not given
-- z :: Int
-- z = frgn5 { a: 3 }



showAll :: forall a. Show a => Array a -> String
showAll values = intercalate ", " (show <$> values)

main :: Effect Unit
main = do
  log $ showAll [x, y]
  log $ showAll [x1, y1]
  log $ showAll [x2, y2, z2, aa2]
  log $ showAll [x3, y3]
  log $ showAll [x4, y4]
  log $ showAll [x5, y5, z5]