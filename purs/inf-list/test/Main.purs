module Test.Main where

import Prelude

import Data.Identity (Identity)
import Data.List.Infinite as InfList
import Data.List.Lazy ((:), nil)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

infListSpecs :: SpecT Aff Unit Identity Unit
infListSpecs = unsafePartial do
  describe "InfList" do
    describe "creation" do
      it "doesn't hang" do
        (InfList.iterate (_ + 1) 0 { maxElements: 10000000 } # InfList.take 5)
          `shouldEqual` (0:1:2:3:4: nil)

    let testList = InfList.iterate (_ + 1) 0 { maxElements: 1000000 }

    describe "find" do
      it "doesn't hang" do
        (testList # InfList.find (_ == 10))
          `shouldEqual` (Just 10)

    describe "index" do
      it "doesn't hang" do
        (testList # InfList.index 100) `shouldEqual` Just 100

    describe "show" do
      it "doesn't hang" do
        show testList `shouldEqual` "InfList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, ...]"

    describe "working with lists of maybes" do
      let 
        even i 
          | i `mod` 2 == 0 = Just i
          | otherwise = Nothing

      describe "mapMaybe" do
        it "doesn't hang" do
          (testList # InfList.mapMaybe even # InfList.take 3) `shouldEqual` (0:2:4:nil)

      describe "catMaybes" do 
        it "doesn't hang" do
          (testList <#> even # InfList.catMaybes # InfList.take 3) `shouldEqual` (0:2:4:nil)

    describe "chunkBySize" do
      it "doesn't hang" do
        (testList # InfList.chunkBySize 2 # InfList.take 3) 
          `shouldEqual` ((0:1:nil) : (2:3:nil) : (4:5:nil) : nil)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  infListSpecs