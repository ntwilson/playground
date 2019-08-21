module Main 
  ( Base
  , baseFromInt
  , Digit
  , digitFromInt
  , baseConverter
  , multBaseConverter
  , main) where

import Prelude

import Data.Array ((..))
import Data.Char (fromCharCode, toCharCode)
import Data.Int as Int
import Data.List.Lazy ((:))
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Data.Maybe.FromJustUnless (fromJustUnless, unsafeFromJustUnless)
import Data.String.CodeUnits (singleton)
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Promise (class Deferred, Promise, promise, runPromise)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion, question, setPrompt)
import Partial.Unsafe (unsafePartial)

newtype Base = Base Int
baseFromInt :: Int -> Maybe Base
baseFromInt i
  | 2 <= i && i <= 62 = Just $ Base i
  | otherwise = Nothing

range :: Base -> Base -> Array Base
range (Base lower) (Base upper) = do
  ibase <- lower .. upper 
  pure $ Base ibase

-- an Int 0 - maxDigit. 10 numeric digits + 26 upper case letter digits + 26 lower case letter digits
newtype Digit = Digit Int
newtype LetterIndex = LetterIndex Int -- 0 - 25 ('a' - 'z')

data DigitRenderer 
  = Direct Int -- 0 - 9
  | Upper LetterIndex  -- 0 - 25 ('A' - 'Z')
  | Lower LetterIndex  -- 0 - 25 ('a' - 'z')

maxDigit :: Int 
maxDigit = 
  let 
    numDirectDigits = 10
    numUpperDigits = 26
    numLowerDigits = 26
  in
    numDirectDigits + numUpperDigits + numLowerDigits - 1

digitFromInt :: Int -> Maybe Digit
digitFromInt i 
  | 0 <= i && i <= maxDigit = Just $ Digit i
  | otherwise = Nothing

renderer :: Digit -> DigitRenderer
renderer (Digit i)
  | 0 <= i && i < 10 = Direct i
  | (let x = i - 10 in 0 <= x && x < 26) = Upper $ LetterIndex $ i - 10
  | otherwise = Lower $ LetterIndex $ i - 10 - 26

renderDigit :: Digit -> String
renderDigit d =
  case renderer d of
    Direct i -> show i
    Upper (LetterIndex i) -> 
      let 
        valOfA = toCharCode 'A'
        charVal = valOfA + i
        char = fromCharCode charVal # unsafeFromJustUnless ("can't add letter index " <> show i <> " to character 'A'")
      in singleton char

    Lower (LetterIndex i) ->
      let 
        valOfA = toCharCode 'a'
        charVal = valOfA + i
        char = fromCharCode charVal # unsafeFromJustUnless ("can't add letter index " <> show i <> " to character 'a'") 
      in singleton char

baseConverter :: Int -> Base -> String 
baseConverter testNum (Base base) = 
  let
    baseDigits = List.iterate (_ * base) base
    relevantDigits = 1 : (baseDigits # List.takeWhile (_ <= testNum)) # List.reverse

    iterate digits (remainder :: Int) =
      case List.uncons digits of
        Just { head: thisDigitRoot, tail: remainingDigits } ->
          let
            thisDigitValue = remainder / thisDigitRoot
            newRemainder = remainder - (thisDigitValue * thisDigitRoot)
            thisDigit = 
              digitFromInt thisDigitValue 
              # unsafeFromJustUnless ("Bad logic computing base convertion for base " <> show base 
                <> ". Remainder " <> show remainder <> " while considering digit " <> show thisDigitRoot 
                <> " resulted in a value that cannot be rendered as a digit: " <> show thisDigitValue)
          in 
            renderDigit thisDigit <> iterate remainingDigits newRemainder
        Nothing -> ""

  in iterate relevantDigits testNum

multBaseConverter :: Int -> Base -> Base -> Array String 
multBaseConverter testNum lBound uBound = do
  base <- range lBound uBound
  pure $ baseConverter testNum base

ask :: Deferred => String -> Promise String
ask prompt = 
  promise \resolve err -> do
    iface <- createConsoleInterface noCompletion
    let 
      resolveAndClose x = do
        resolve x
        close iface

    question prompt resolveAndClose iface


getInput :: Partial => Deferred => Promise { testNum :: Int, lBound :: Base, uBound :: Base }
getInput = do
  testNumStr <- ask "please enter a number to convert: "
  let testNum = fromJustUnless "Unrecognized input.  Expecting a number." $ Int.fromString testNumStr

  lBoundStr <- ask "please enter a lower-bound base:  " 
  let 
    lBound = 
      fromJustUnless "Unrecognized input. Expecting a number between 2 and 62" $ 
        Int.fromString lBoundStr >>= baseFromInt

  uBoundStr <- ask "please enter an upper-bound base: "
  let 
    uBound = 
      fromJustUnless "Unrecognized input. Expecting a number between 2 and 62" $ 
        Int.fromString uBoundStr >>= baseFromInt

  pure { testNum, lBound, uBound }

main :: Effect Unit
main = unsafePartial $
  runPromise 
    (\{ testNum, lBound, uBound } -> do
      logShow $ multBaseConverter testNum lBound uBound)
    (\err -> logShow err)
    getInput

