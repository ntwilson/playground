module Main where

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

import Effect.Console (logShow)
import Effect.Promise (class Deferred, Promise, promise, runPromise)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion, question, setPrompt)
import Partial.Unsafe (unsafePartial)

baseConverter :: Int -> Int -> String 
baseConverter testNum base = 
  let
    baseDigits = List.iterate (_ * base) base
    relevantDigits = 1 : (baseDigits # List.takeWhile (_ <= testNum)) # List.reverse
    render i
      | 0 <= i && i < 10 = show i
      | 10 <= i && i <= 35 = 
        let 
          valOfA = toCharCode 'A'
          charVal = valOfA + (i - 10)
          char = fromCharCode charVal # unsafeFromJustUnless ("unable to add " <> show (i - 10) <> " to character 'A'")
        in singleton char

      | otherwise =
        let 
          valOfA = toCharCode 'a'
          charVal = valOfA + (i - 36)
          char = fromCharCode charVal # unsafeFromJustUnless ("Given too large of a digit: " <> show i <> ". Must be able to add " <> show (i - 37) <> " to character 'a'")
        in singleton char

    iterate digits (remainder :: Int) =
      case List.uncons digits of
        Just { head: nextDigit, tail: remainingDigits } ->
          let
            thisDigitValue = remainder / nextDigit
            newRemainder = remainder - (thisDigitValue * nextDigit)
            thisDigit = render thisDigitValue
          in 
            thisDigit <> iterate remainingDigits newRemainder
        Nothing -> ""

  in iterate relevantDigits testNum

multBaseConverter :: Int -> Int -> Int -> Array String 
multBaseConverter testNum lBound uBound = do
  base <- lBound .. uBound
  pure $ baseConverter testNum base

ask :: Deferred => String -> Interface -> Promise String
ask prompt iface = 
  promise \resolve err -> question prompt resolve iface

fromJust :: forall a. Partial => Maybe a -> a
fromJust = fromJustUnless "Unrecognized input.  Expecting a number."

getInput :: Partial => Deferred => Interface -> Promise { lBound :: Int, testNum :: Int, uBound :: Int }
getInput iface = do
  testNumStr <- ask "please enter a number to convert: " iface
  let testNum = fromJust $ Int.fromString testNumStr
  lBoundStr <- ask "please enter a lower-bound base:  " iface
  let lBound = fromJust $ Int.fromString lBoundStr
  uBoundStr <- ask "please enter an upper-bound base: " iface
  let uBound = fromJust $ Int.fromString uBoundStr
  pure { testNum, lBound, uBound }

main :: Effect Unit
main = unsafePartial do
  iface <- createConsoleInterface noCompletion
  setPrompt "> " 2 iface
  
  runPromise 
    (\{ testNum, lBound, uBound } -> do
      logShow $ multBaseConverter testNum lBound uBound
      close iface)
    (\_ -> close iface)
    (getInput iface)

