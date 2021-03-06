module Test.Main where

import Prelude

import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Maybe.FromJustUnless (fromJustUnless, unsafeFromJustUnless)
import Data.String.CodeUnits (length)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Main (Base, Digit, NaturalInt, baseConverter, baseFromInt, digitFromInt, multBaseConverter, naturalIntFromInt, renderDigit)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary, (===))
import Test.QuickCheck.Gen (chooseInt, suchThat)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

mkBase :: Partial => Int -> Base
mkBase i = baseFromInt i # fromJustUnless ("bad test. Trying to create a Base of " <> show i)

baseConversionSpecs :: SpecT Aff Unit Identity Unit
baseConversionSpecs = unsafePartial do
  describe "number base conversions" do
    describe "with a single target base" do
      it "converts a decimal number to the supplied base" do
        baseConverter (naturalIntFromInt 5) (mkBase 2) `shouldEqual` "101"
        baseConverter (naturalIntFromInt 11) (mkBase 2) `shouldEqual` "1011"
        baseConverter (naturalIntFromInt 5) (mkBase 3) `shouldEqual` "12"
        baseConverter (naturalIntFromInt 7) (mkBase 3) `shouldEqual` "21"

      it "handles converting 0" do
        baseConverter (naturalIntFromInt 0) (mkBase 2) `shouldEqual` "0"
        baseConverter (naturalIntFromInt 0) (mkBase 5) `shouldEqual` "0"
    
    describe "with multiple target bases" do
      it "converts to all bases between an upper and lower bound" do
        multBaseConverter 7 (mkBase 2) (mkBase 8) `shouldEqual` ["111","21","13","12","11","10","7"]
        multBaseConverter 0 (mkBase 2) (mkBase 8) `shouldEqual` [  "0", "0", "0", "0", "0", "0","0"]

    describe "with bases that use all available character sets" do
      it "gets the boundaries correct" do
        baseConverter (naturalIntFromInt 9) (mkBase 40) `shouldEqual` "9"
        baseConverter (naturalIntFromInt 10) (mkBase 40) `shouldEqual` "A"
        baseConverter (naturalIntFromInt 35) (mkBase 40) `shouldEqual` "Z"
        baseConverter (naturalIntFromInt 36) (mkBase 40) `shouldEqual` "a"
        baseConverter (naturalIntFromInt 61) (mkBase 62) `shouldEqual` "z"

  describe "acceptance criteria" do
    it "gives the correct answer for the problems provided in the writeup" do
      multBaseConverter 10 (mkBase 2) (mkBase 4) `shouldEqual` ["1010", "101", "22"]
      multBaseConverter 100 (mkBase 15) (mkBase 20) `shouldEqual` ["6A", "64", "5F", "5A", "55", "50"]

newtype ArbDigit = ArbDigit Digit

instance digitArb :: Arbitrary ArbDigit where
  arbitrary = do
    d <-
      suchThat 
        (map digitFromInt (chooseInt 0 64))
        (case _ of 
          Nothing -> false 
          Just _ -> true)
      
    pure $ ArbDigit $ unsafeFromJustUnless "" d

newtype ArbBase = ArbBase Base

instance baseArb :: Arbitrary ArbBase where
  arbitrary = do
    b <-
      suchThat 
        (map baseFromInt (chooseInt 0 64))
        (case _ of 
          Nothing -> false 
          Just _ -> true)
      
    pure $ ArbBase $ unsafeFromJustUnless "" b

newtype ArbNaturalInt = ArbNaturalInt NaturalInt
instance arbNaturalInt :: Arbitrary ArbNaturalInt where
  arbitrary = map (\i -> ArbNaturalInt (naturalIntFromInt i)) arbitrary

baseConversionProperties :: SpecT Aff Unit Identity Unit
baseConversionProperties = do
  describe "renderDigit" do
    it "does not crash for any inputs" do
      quickCheck \(ArbDigit d) -> (renderDigit d # length) === 1

  describe "baseConverter" do
    it "does not crash for any inputs" do 
      quickCheck \(ArbNaturalInt testNum) (ArbBase base) -> (baseConverter testNum base # length) > 0


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  baseConversionSpecs
  baseConversionProperties
