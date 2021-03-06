module Main 
  ( Base
  , NaturalInt 
  , naturalIntFromInt
  , baseFromInt
  , Digit
  , digitFromInt
  , renderDigit
  , baseConverter
  , multBaseConverter
  , main) where

import Prelude

import Data.Array ((..))
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Int as Int
import Data.List.Lazy ((:))
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Data.Maybe.FromJustUnless (fromJustUnless, unsafeFromJustUnless)
import Data.String.CodeUnits (singleton)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion, question)
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
    numTotalDigits = numDirectDigits + numUpperDigits + numLowerDigits
  in
    numTotalDigits - 1

digitFromInt :: Int -> Maybe Digit
digitFromInt i 
  | 0 <= i && i <= maxDigit = Just $ Digit i
  | otherwise = Nothing

renderer :: Digit -> DigitRenderer
renderer (Digit i) = ans
  where
    ans
      | 0 <= i && i < 10 = Direct i
      | 0 <= upperIndex && upperIndex < 26 = Upper $ LetterIndex $ upperIndex
      | otherwise = Lower $ LetterIndex $ lowerIndex

    upperIndex = i - 10
    lowerIndex = upperIndex - 26

renderDigit :: Digit -> String
renderDigit d = case renderer d of
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

newtype NaturalInt = NaturalInt Int
naturalIntFromInt :: Int -> NaturalInt
naturalIntFromInt i 
  | i < 0 = NaturalInt 0
  | otherwise = NaturalInt i

baseConverter :: NaturalInt -> Base -> String 
baseConverter (NaturalInt testNum) (Base base) = 
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
                <> " with number " <> show testNum
                <> ". Remainder " <> show remainder <> " while considering digit " <> show thisDigitRoot 
                <> " resulted in a value that cannot be rendered as a digit: " <> show thisDigitValue)
          in 
            renderDigit thisDigit <> iterate remainingDigits newRemainder
        Nothing -> ""

  in iterate relevantDigits testNum

multBaseConverter :: Int -> Base -> Base -> Array String 
multBaseConverter testNum lBound uBound = do
  base <- range lBound uBound
  let n = naturalIntFromInt testNum
  pure $ baseConverter n base

ask :: Interface -> String -> Aff String
ask iface prompt = 
  makeAff \resolveWithEither -> do
    let resolve x = resolveWithEither (Right x)

    question prompt resolve iface
    mempty


getInput :: Partial => Aff { testNum :: Int, lBound :: Base, uBound :: Base }
getInput = do
  iface <- liftEffect $ createConsoleInterface noCompletion 
  let ask' = ask iface 

  testNumStr <- ask' "please enter a number to convert: "
  let testNum = fromJustUnless "Unrecognized input.  Expecting a number." $ Int.fromString testNumStr

  lBoundStr <- ask' "please enter a lower-bound base:  " 
  let 
    lBound = 
      fromJustUnless "Unrecognized input. Expecting a number between 2 and 62" $ 
        Int.fromString lBoundStr >>= baseFromInt

  uBoundStr <- ask' "please enter an upper-bound base: "
  let 
    uBound = 
      fromJustUnless "Unrecognized input. Expecting a number between 2 and 62" $ 
        Int.fromString uBoundStr >>= baseFromInt

  liftEffect $ close iface 
  pure { testNum, lBound, uBound }

main :: Effect Unit
main = unsafePartial $ launchAff_ do
  { testNum, lBound, uBound } <- getInput
  liftEffect $ logShow $ multBaseConverter testNum lBound uBound
