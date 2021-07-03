module Main where

import Prelude

import Data.Formatter.Number (formatNumber)
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  logShow $ formatNumber "0.0" 1234567890123456789.0
