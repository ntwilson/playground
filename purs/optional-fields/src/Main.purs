module Main where

import Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prim.Row (class Union)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- This codebase documents several approaches for dealing with JS FFI where JS
-- expects an object with optional properties.  This codebase assumes beginning-level understanding
-- of PureScript or even of Haskell-inspired languages and contains comments that describe the fundamental
-- workings of the language.  If PureScript is already familiar to the reader, I apologize that
-- many of the comments will just be noise.  That said: please do not take this as "normal" use of 
-- type classes.  The explanations in here will not help you understand how type classes are normally
-- used.  You should look at the PureScript book (https://leanpub.com/purescript/read#leanpub-auto-type-classes)
-- for that.
  
-- Don't forget, the raw JS code is available in Main.js
-- in case you want to see what the implementation of these foreign JS functions are.
-- 
-- The methods of handling optional properties are listed pretty much in the order in which they were 
-- written as I played around with it, _not_ in order of complexity.  Especially if you are relatively
-- unfamiliar with PureScript, I do not recommend reading it top down, as the most complex approaches 
-- are at the top.  I would perhaps recommend bottom up, or perhaps specifically in order 4,3,1,2,0,5
-- (which I think is a good ranking based on both increasing complexity and decreasing likely usefulness)
-- 
-- Methods 0 - 3 show how to actually encode records with optional properties in the 
-- Purescript type system.  These approaches are the safest and most convenient to consume
-- but involve some type class & constraint sorcery that can be more challenging to understand
-- than the other methods.
-- Methods 4 & 5 are much simpler, but have drawbacks.  Method 4 is more boilerplate to write & consume
-- and will ultimately set all optional fields to undefined, which in JS is subtly different than
-- if the field isn't there at all (though there is a way to work around this).  
-- Method 5 simply abandons any typechecking of the optional properties, 
-- and allows the caller to pass in whatever additional properties they desire.
-- 
-- Method 0: Supports required and optional fields, but requires some duplication in that 
-- you must define the required fields in one type definition, and also all required + optional
-- fields in another type definition
-- 
-- Method 1: Supports required and optional fields, and makes passing the structure around from
-- function to function significantly easier than methods 0, 2, and 3, at the cost of adding a 
-- constructor function for the record, so that you have to call the constructor rather than 
-- passing a raw record into the foreign function.
-- 
-- Method 2: Does not support required fields.  The complexity goes down significantly, but
-- is only usable when no fields are required and an empty object is a valid input.  This implementation
-- loosely follows the same strategy as method 0, but could be adapted to simplify methods 1 and 3 
-- if using those methods in a situation that does not use required fields.
-- 
-- Method 3: Supports required and optional fields, but defines the 
-- optional constraints as part of the imported function definition.  
-- This is the least complex method of supporting required and optional fields, 
-- but if more than one function work with the same type of record, 
-- the constraints would need to be repeated on each function definition, which arguably could
-- cause more confusion than a type class constraint.
-- 
-- Method 4: A different approach to optional properties, where they are treated as not optional
-- as far as the purescript compiler is concerned, and we use native (and simple) purescript strategies
-- to set the defaults for each optional property manually to `undefined`. (Note that this would 
-- produce a different behavior if the JS code treats a missing property on an object differently 
-- than it would treat an object with a property that equals undefined.  This can be worked around with
-- additional complexity.  An example of that is here: https://blog.ndk.io/purescript-ffi.html) 
-- 
-- Method 5: The simplest approach, but not as safe as the others. Only defines required fields 
-- (if there are any), but allows the caller to pass in any additional fields they want to.
-- This allows for any optional fields to be passed in, but the typechecker only checks to 
-- make sure the required fields are present.  It doesn't ensure that any other fields are 
-- recognized.  This probably isn't suitable for a production PureScript application, but 
-- is also probably the go-to approach when prototyping something.
--
-- I suspect that methods 3, 4 & perhaps 1 are the most broadly useful across many different
-- scenarios, though each has unique advantages.



--------------------------------------------------------------------------------------
-- METHOD 0
-- Define a type (well, a type class) with Required fields and the entire set of required + optional
-- fields that you could use from many functions.  This is one of the most complicated methods of optional 
-- fields.
--------------------------------------------------------------------------------------

type Required = (b :: Int)
-- a "row" containing the required fields + any other fields we'll call `optionalGiven`.
type RequiredPlus optionalGiven = (b :: Int | optionalGiven)

-- with RequiredPlus defined, we could also define Required as:
-- type Required = RequiredPlus ()
-- here, by passing unit `()` to RequiredPlus, this is only the required fields.

type Optional = (a :: Int)

-- the sum of the required fields and the optional fields, yielding all allowed fields.
-- the `+` operator used here takes in an "open" row (e.g., `forall r. ( x :: Int | r)`)
-- on the left, and a "closed" row (e.g., `(x :: Int)`) on the right, which is why we 
-- use RequiredPlus on the left (RequiredPlus is open because it also includes any optionalGiven)
type AllFields = RequiredPlus + Optional

-- an empty type class.  Kind of like an empty interface from other languages that you would expect to apply to 
-- classes in order to make some assertion about that class.  In this case, the empty type class will be applied
-- to types that match our required & optional field requirements.  This type class is "generic" on 3 variable types
class Method0Rec (given :: #Type) (rest :: #Type) (optionalGiven :: #Type) 

-- an instance of the Method0Rec type class for _any_ variable type `given` provided that it will satisfy our 
-- constraints.  (Determining whether or not `given` satisfies our constraints means we need to solve for 2 other
-- types that we don't actually care much about, the remainder of AllFields that was not included in `given`, 
-- called `rest`, and whatever optional fields were part of `given` but are not required, called `optionalGiven`).  
-- We supply 2 constraints here, both using the Union type class.  Union means that the fields in the first
-- two types must strictly add together to equal the third type.  Unlike a mathematical union, this includes 
-- duplicates, so {a, b} {b, c} does _not_ union to {a, b, c} because b is duplicated.  Union has a special
-- property that if you know any 2 of the types, you can solve for the third.  
instance method0Rec :: 
  ( Union given rest AllFields -- given must be a subset of AllFields (given + rest = AllFields)
  , Union Required optionalGiven given -- given must be a superset of Required (Required + any optional given = given)
    -- in the first constraint, we expect to know what is `given`, and we obviously know AllFields, so we solve for `rest`
    -- in the second constraint, we obviously know Required, and we know what is `given`, so we solve for `optionalGiven`
    -- if we can solve for `rest` and `optionalGiven` and both Union constraints still hold true, then this type class
    -- instance applies to the types you give it.  If no solution exists, then the instance does not apply.
  ) => Method0Rec given rest optionalGiven

-- frgn is "generic" on 3 type variables, which all get infered.  frgn has a constraint that there must exist an
-- instance of the Method0Rec type class for all the type variables (and to prove that the instance exists for
-- `given`, the compiler needs to solve the other two type variables).
-- finally, the signature of the function is simply `(Record given) -> Int`
foreign import frgn :: 
  forall given rest optionalGiven. 
  Method0Rec given rest optionalGiven => 
  (Record given) -> Int

x :: Int 
x = frgn { b: 5 }

y :: Int
y = frgn { a: 5, b: 3 }


-- does not compile because `b` is required but not given
-- z :: Int
-- z = frgn { a: 3 }

-- does not compile because `t` is given but not required or optional
-- z :: Int
-- z = frgn { b: 3, t: 5 }

--------------------------------------------------------------------------------------
-- METHOD 1
-- Define a foreign type plus a constructor that defines the required and optional fields
-- the same way as Method 3.  This adds the overhead of a constructor, but allows you
-- to pass around the record from function to function once constructed without needing
-- to add constraints to each function that handles it.
-- Contrast this with methods 3, 0, and 2, which require constraints on any function
-- that works with the record type.
-- Method 1 also has the additional (though minor) benefit that the foreign function being
-- imported keeps a very simple definition, and it requires no special modifications in 
-- Main.js to be able to call it.
--------------------------------------------------------------------------------------

-- references RequiredPlus from above, copied here for convenience:
-- -- A record that must include (b :: Int) (which is the required field(s)), 
-- -- but then can include other fields which we will together call `optionalGiven`
-- type RequiredPlus optionalGiven = ( b :: Int | optionalGiven )

-- references Optional from above, copied here for convenience
-- type Optional = (a :: Int)

-- A phantom type to represent the object being passed in.  By making it a foreign import, we 
-- can require constraints to construct this type, but then discard all of the type variables and 
-- their associated constraints, letting us pass this type around without complicating our function
-- signatures.  (Note that most foreign
-- imports must have a corresponding binding in the .js file for the entity being imported, but
-- a `foreign import data` is just an assertion that some type exists that you will be using in
-- subsequent bindings, so there is no type definition in the .js file to be imported)
foreign import data Method1Rec :: Type

-- A constructor for a Method1Rec.  What gets passed into this function  must actually be a valid record
-- for our foreign import, but once we've solved the constraints, we can just "assert" that it is indeed a 
-- Method1Rec with `unsafeCoerce`, and then discard all of the type variables needed to solve the constraints.
-- mkMethodRec1 is "generic" on 2 type variables, which both get infered.  mkMethodRec1 has a constraint for the variable types
-- using the Union type class.  Union means that the fields in the first
-- two types must strictly add together to equal the third type.  Unlike a mathematical union, this includes 
-- duplicates, so {a, b} {b, c} does _not_ union to {a, b, c} because b is duplicated.  Union has a special
-- property that if you know any 2 of the types, you can solve for the third.
-- (Determining whether or not `optionalGiven` satisfies our constraint means we need to solve for another variable
-- type that we don't actually care much about, the remainder of Optional that was not included in `optionalGiven`, called `rest`).  
-- The actual signature of the function is `AllFields3 optionalGiven -> Method1Rec`, where AllFields3 defined below determines
-- what `optionalGiven` means, and `optionalGiven` is then what gets used to solve our Union constraint. 
mkMethod1Rec ::
  forall optionalGiven rest.
    Union optionalGiven rest Optional -- optionalGiven must be a subset of Optional (optionalGiven + rest = Optional)
      -- in this constraint, we expect to know what is `optionalGiven`, and we obviously know Optional, so we solve for `rest`
      -- if we can solve for `rest` and the Union constraint still holds true, then the constraint is satisfied.  
      -- If no solution exists, then the type variables used are invalid (and we solved for `rest` so really just 
      -- `optionalGiven` is invalid).
    => 
    Record (RequiredPlus optionalGiven) -> Method1Rec
mkMethod1Rec = unsafeCoerce
  

-- frgn1 now relies on the constraints baked into the constructor for a Method1Rec,
-- and the actual definition of frgn1 is quite simple.
foreign import frgn1 :: Method1Rec -> Int

x1 :: Int 
x1 = frgn1 $ mkMethod1Rec { b: 5 }

y1 :: Int
y1 = frgn1 $ mkMethod1Rec { a: 5, b: 3 }

-- does not compile because `b` is required but not given
-- z :: Int
-- z = frgn1 $ mkMethod1Rec { a: 3 }

-- does not compile because `t` is given but not required or optional
-- z :: Int
-- z = frgn1 $ mkMethod1Rec { b: 3, t: 5 }


--------------------------------------------------------------------------------------
-- METHOD 2
-- Define a type (well, a type class) with Optional fields that you 
-- could use from many functions.  This can be used when _all_ fields have defaults,
-- and it would be valid to pass in a completely empty record.  This is a much simpler
-- solution than method 0 for cases where you don't have required fields.  This implementation
-- loosely follows the same strategy as method 0, but could be adapted to simplify methods 1 and 3 
-- if using those methods in a situation that does not use required fields.
--------------------------------------------------------------------------------------

-- references AllFields from above, copied here for convenience
-- type AllFields = (a :: Int, b :: Int)

-- an empty type class.  Kind of like an empty interface from other languages that you would expect to apply to 
-- classes in order to make some assertion about that class.  In this case, the empty type class will be applied
-- to types that match our optional field requirements.  This type class is "generic" on 2 variable types
class Method2Rec (given :: #Type) (rest :: #Type)

-- an instance of the Method2Rec type class for _any_ variable type `given` provided that it will satisfy our 
-- constraints.  (Determining whether or not `given` satisfies our constraints means we need to solve for one other variable
-- type that we don't actually care much about, the remainder of AllFields that was not included in `given`, called `rest`).  
-- We supply one constraint here using the Union type class.  Union means that the fields in the first
-- two types must strictly add together to equal the third type.  Unlike a mathematical union, this includes 
-- duplicates, so {a, b} {b, c} does _not_ union to {a, b, c} because b is duplicated.  Union has a special
-- property that if you know any 2 of the types, you can solve for the third.
instance method2Rec :: 
  (Union given rest AllFields -- given must be a subset of AllFields (given + rest = AllFields)
    -- in this constraint, we expect to know what is `given`, and we obviously know AllFields, so we solve for `rest`
    -- if we can solve for `rest` and the Union constraint still holds true, then this type class
    -- instance applies to the types you give it.  If no solution exists, then the instance does not apply.
  ) => Method2Rec given rest


-- frgn2 is "generic" on 2 type variables, which both get infered.  frgn2 has a constraint that there must exist an
-- instance of the Method2Rec type class for all the type variables (and to prove that the instance exists for
-- `given`, the compiler needs to solve the other variable `rest`).
-- finally, the signature of the function is simply `(Record given) -> Int`
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

-- does not compile because `t` is given but not allowed in AllFields
-- ab2 :: Int
-- ab2 = frgn2 { a: 5, t: 2 }

--------------------------------------------------------------------------------------
-- METHOD 3
-- Define a function whose arguments include required fields, and given a direct constraint
-- for optional fields.  This supports required + optional fields like methods 0 & 1, but 
-- is a much simpler approach than those methods for cases where the record type with optional fields 
-- is only needed by a single function, or it is simple to redefine constraints on
-- each function using that record.  (Although remember: the PureScript compiler can infer
-- function constraints if you omit a function's signature.  It will produce a warning if your function 
-- doesn't have a signature, but offer an auto-fix where it adds in the infered signature).
--------------------------------------------------------------------------------------

-- references RequiredPlus from above, copied here for convenience
-- -- a "row" containing the required fields + any other fields we'll call `optionalGiven`.
-- type RequiredPlus optionalGiven = (b :: Int | optionalGiven)

-- references Optional from above, copied here for convenience
-- type Optional = (a :: Int) 

-- frgn3 is "generic" on 2 type variables, which both get infered.  frgn3 has a constraint for the variable types
-- using the Union type class.  Union means that the fields in the first
-- two types must strictly add together to equal the third type.  Unlike a mathematical union, this includes 
-- duplicates, so {a, b} {b, c} does _not_ union to {a, b, c} because b is duplicated.  Union has a special
-- property that if you know any 2 of the types, you can solve for the third.
-- (Determining whether or not `optionalGiven` satisfies our constraint means we need to solve for another variable
-- type that we don't actually care much about, the remainder of Optional that was not included in `optionalGiven`, called `rest`).  
-- The actual signature of the function is `AllFields3 optionalGiven -> Int`, where AllFields3 defined above determines
-- what `optionalGiven` means, and `optionalGiven` is then what gets used to solve our Union constraint.
foreign import frgn3 ::
  forall optionalGiven rest.
  Union optionalGiven rest Optional -- optionalGiven must be a subset of Optional (optionalGiven + rest = Optional)
    -- in this constraint, we expect to know what is `optionalGiven`, and we obviously know Optional, so we solve for `rest`
    -- if we can solve for `rest` and the Union constraint still holds true, then the constraint is satisfied.  
    -- If no solution exists, then the type variables used are invalid (and we solved for `rest` so really just 
    -- `optionalGiven` is invalid).
  => 
  Record (RequiredPlus optionalGiven) -> Int

x3 :: Int 
x3 = frgn3 { b: 5 }

y3 :: Int
y3 = frgn3 { a: 5, b: 3 }

-- does not compile because `b` is required but not given
-- z :: Int
-- z = frgn3 { a: 3 }

-- does not compile because `t` is given but not required or optional
-- z :: Int
-- z = frgn3 { b: 3, t: 5 }


--------------------------------------------------------------------------------------
-- METHOD 4
-- Define a type that includes optional fields as Maybes, and a constructor function
-- that defaults each optional field to Nothing.  
-- This is a much simpler approach than any of the above methods, but requires more
-- boilerplate, and could introduce subtle bugs if the JS code being called treats 
-- an existing field that is undefined differently than a field that is not present.
-- This can be worked around with additional complexity.  
-- An example of that is here: https://blog.ndk.io/purescript-ffi.html
-- 
-- Another advantage to this approach is that the optional fields can be consumed by
-- PureScript code where it isn't known by the compiler if the optional value is present
-- or not.
-- Note that in this example, we're using `undefined` as the JS default value for any
-- optional fields, but you could use this technique to apply any default you want.
--------------------------------------------------------------------------------------

-- this stuff at the top is for working with JS undefined.  It would only need to be defined
-- once, not for each type that has optional fields.

-- `Undefined` here means pretty much the same thing as Maybe.  You can have an `Undefined Int`
-- which is something that might be an Int, or might be undefined.  (Note that most foreign
-- imports must have a corresponding binding in the .js file for the entity being imported, but
-- a `foreign import data` is just an assertion that some type exists that you will be using in
-- subsequent bindings, so there is no type definition in the .js file to be imported)
foreign import data Undefined :: Type -> Type
-- `undefined` here is the raw JS value for undefined, but we supply a type variable for it
-- since it could be standing in for any type.  Much like `Nothing` can be used as a `Maybe Int` 
-- or as a `Maybe String`, so `undefined` could be used as an `Undefined Int` or as an `Undefined String`
foreign import undefined :: forall a. Undefined a

-- our runtime implementation is just the plain value or else undefined.  At runtime, there is
-- no difference between an `Undefined Int`, and just `Int`, we're just tracking with the compiler
-- that it _could_ be undefined.  Hence the `unsafeCoerce` of an `a` to an `Undefined a`.
maybeToUndefined :: forall a. Maybe a -> Undefined a
maybeToUndefined (Just v) = unsafeCoerce v  
maybeToUndefined Nothing = undefined

-- references Required above, copied here for convenience:
-- type Required = (b :: Int)

-- frgn4Impl would be hidden external to this module, only the wrapper function frgn4 defined below
-- would be accessible publicly.  frgn4Impl will _always_ be given a record of both `a` and `b`, 
-- but `a` might be `undefined`
foreign import frgn4Impl :: { a :: Undefined Int | Required } -> Int

type AllFieldsWithMaybeDefaults = { a :: Maybe Int | Required } 

-- we take in the required fields as the first argument (through which we'll construct a record
-- that contains both required _and_ optional fields, with the optional fields given default values),
-- and then we take in a lambda to apply any changes to the optional fields.
frgn4 :: Record Required -> (AllFieldsWithMaybeDefaults -> AllFieldsWithMaybeDefaults) -> Int
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
-- syntactic sugar for `frgn4 {b: 3} (\x -> x {a = Just 5})`.  So a lambda + the record copy&update syntax
-- note the `=` instead of a `:` in the copy&update.  That was a detail I initially overlooked.



--------------------------------------------------------------------------------------
-- METHOD 5
-- Define the foreign function by only providing the required fields.  The typechecker 
-- simply ignores any optional fields.  This means that if you pass in a field that
-- isn't recognized by the JS code, it will just be ignored and there will be no 
-- compile error.  
-- This is the absolute simplest approach, at the cost of some typechecking that you
-- get with the other approaches.
--------------------------------------------------------------------------------------

-- frgn5 takes in a record that must include (b :: Int), but may also include any other 
-- fields, and we'll call all the extra fields `r`.  If I had left off the `| r`, then 
-- this signature would only accept records that contained exactly (b :: Int) and 
-- nothing else.
foreign import frgn5 :: forall r. { b :: Int | r } -> Int

x5 :: Int 
x5 = frgn5 { b: 5 }

y5 :: Int
y5 = frgn5 { a: 5, b: 3 }

-- compiles even though `t` is not recognized
z5 :: Int
z5 = frgn5 { b: 3, t: 5 }

-- does not compile because `b` is required but not given
-- z :: Int
-- z = frgn5 { a: 3 }


-- so provided that whatever type `a` is fits our constraint (there must exist an instance
-- of the `Show` type class for type `a`, which defines the `show` function), then this function
-- is just `Array a -> String`
showAll :: forall a. Show a => Array a -> String
showAll values = intercalate ", " (show <$> values)

main :: Effect Unit
main = do
  log $ showAll [x, y] -- method0
  log $ showAll [x1, y1] -- method1
  log $ showAll [x2, y2, z2, aa2] -- method2
  log $ showAll [x3, y3] -- method3
  log $ showAll [x4, y4] -- method4
  log $ showAll [x5, y5, z5] -- method5

