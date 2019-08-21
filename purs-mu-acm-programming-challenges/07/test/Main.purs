module Test.Main where

import Prelude

import Data.List ((..), (:), List(..))
import Data.List as List
import Control.Monad.Error.Class (class MonadThrow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error)
import Main (chunkBySize, encrypt)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

chunkSpecs :: forall a m. Monad m => MonadThrow Error a => SpecT a Unit m Unit
chunkSpecs = do
  describe "chunkBySize" do
    describe "given a perfect divisor" do
      it "produces a square matrix" do
        chunkBySize 4 (1 .. 12) `shouldEqual` ((1..4) : (5..8) : (9..12) : Nil)

    describe "given a divisor with a remainder" do
      it "includes the remainder in the final element of the not-square result" do
        chunkBySize 4 (1 .. 11) `shouldEqual` ((1..4) : (5..8) : (9..11) : Nil)

    describe "given an empty array" do
      it "returns an empty array" do
        chunkBySize 4 (Nil :: List Int) `shouldEqual` Nil

    describe "given an array already of length = chunk size" do
      it "returns the same as the input" do
        chunkBySize 4 (1 .. 4) `shouldEqual` (List.singleton (1 .. 4))

encryptionSpecs :: forall a m. Monad m => MonadThrow Error a => SpecT a Unit m Unit
encryptionSpecs = do
  describe "acceptance criteria" do
    describe "encrypting 'i am really enjoying my time at steerbucks this fine wednesday morning'" do
      it "produces the same encrypted string as the the problem description" do
        encrypt "i am really enjoying my time at steerbucks this fine wednesday morning" 
          `shouldEqual` "iygtcisi aemskndn mnytseag rjtetwy eoiehem aymrido liebsnr lnaufen"

    describe "encrypting 'my name is sam'" do 
      it "produces the same encrypted string as the the problem description" do
        encrypt "my name is sam" 
          `shouldEqual` "mms yea nim as"

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  chunkSpecs
  encryptionSpecs
