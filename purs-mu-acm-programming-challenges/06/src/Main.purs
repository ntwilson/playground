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
import Node.ReadLine (close, createConsoleInterface, noCompletion, question, setPrompt)
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

main :: Effect Unit
main = unsafePartial do
  iface <- createConsoleInterface noCompletion
  let 
    qstn prompt resolve = question prompt resolve iface
    fromJust = fromJustUnless "Unrecognized input.  Expecting a number."
  setPrompt "> " 2 iface
  qstn "please enter a number to convert: " (\testNumStr -> do 
    let testNum = fromJust $ Int.fromString testNumStr
    qstn "please enter a lower-bound base:  " (\lBoundStr -> do
      let lBound = fromJust $ Int.fromString lBoundStr
      qstn "please enter an upper-bound base: " (\uBoundStr -> do
        let uBound = fromJust $ Int.fromString uBoundStr
        logShow $ multBaseConverter testNum lBound uBound
        close iface
  )))
