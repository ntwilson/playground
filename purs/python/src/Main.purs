module Main where

import Prelude

import Effect (Effect)
import Greeter (sayHi)

main :: Effect Unit
main = sayHi
