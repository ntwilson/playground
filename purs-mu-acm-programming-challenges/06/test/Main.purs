module Test.Main where

import Prelude

import Data.Identity (Identity)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Main (baseConverter, multBaseConverter)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

refreshingPlotSpecs :: SpecT Aff Unit Identity Unit
refreshingPlotSpecs = do
  describe "number base conversions" do
    describe "with a single target base" do
      it "converts a decimal number to the supplied base" do
        baseConverter 5 2 `shouldEqual` "101"
        baseConverter 11 2 `shouldEqual` "1011"
        baseConverter 5 3 `shouldEqual` "12"
        baseConverter 7 3 `shouldEqual` "21"

      it "handles converting 0" do
        baseConverter 0 2 `shouldEqual` "0"
        baseConverter 0 5 `shouldEqual` "0"
    
    describe "with multiple target bases" do
      it "converts to all bases between an upper and lower bound" do
        multBaseConverter 7 2 8 `shouldEqual` ["111","21","13","12","11","10","7"]
        multBaseConverter 0 2 8 `shouldEqual` [  "0", "0", "0", "0", "0", "0","0"]

    describe "with bases that use all available character sets" do
      it "gets the boundaries correct" do
        baseConverter 9 40 `shouldEqual` "9"
        baseConverter 10 40 `shouldEqual` "A"
        baseConverter 35 40 `shouldEqual` "Z"
        baseConverter 36 40 `shouldEqual` "a"

  describe "acceptance criteria" do
    it "gives the correct answer for the problems provided in the writeup" do
      multBaseConverter 10 2 4 `shouldEqual` ["1010", "101", "22"]
      multBaseConverter 100 15 20 `shouldEqual` ["6A", "64", "5F", "5A", "55", "50"]


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  refreshingPlotSpecs
