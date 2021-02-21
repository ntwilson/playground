module Test.Main where

import Prelude

import Data.Either (Either(..), isLeft, isRight)
import Data.List.Infinite (ApplicationHung(..), InfList)
import Data.List.Infinite as InfList
import Data.List.Lazy ((:), nil)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

wellFormedList :: InfList Int
wellFormedList = InfList.iterate (_ + 1) 0 { maxElements: 10000 }

illFormedList :: InfList Int
illFormedList = InfList.iterate (const 0) 0 { maxElements: 1000 } # InfList.filter (_ /= 0)

infListSpecs :: Spec Unit
infListSpecs = unsafePartial do
  describe "InfList" do
    describe "show" do
      it "doesn't hang" do
        show wellFormedList `shouldEqual` "InfList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, ...]"
        show illFormedList `shouldEqual` "InfList <application hung while showing the list>"
      
    describe "take" do
      it "doesn't hang" do
        (wellFormedList # InfList.take 5)
          `shouldEqual` Right (0:1:2:3:4: nil)

        (illFormedList # InfList.take 5) 
          `shouldSatisfy` isLeft

    describe "takeLazy" do
      it "doesn't hang" do
        (wellFormedList # InfList.takeLazy 5)
          `shouldEqual` (Right 0 : Right 1 : Right 2 : Right 3 : Right 4 : nil)

        (illFormedList # InfList.takeLazy 5)
          `shouldEqual` (Left ApplicationHung : nil)

    describe "takeWhile" do
      it "doesn't hang" do
        (wellFormedList # InfList.takeWhile (_ < 5))
          `shouldEqual` Right (0:1:2:3:4: nil)

        (illFormedList # InfList.takeWhile (_ < 5))
          `shouldSatisfy` isLeft

    describe "takeWhileLazy" do
      it "doesn't hang" do
        (wellFormedList # InfList.takeWhileLazy (_ < 5))
          `shouldEqual` (Right 0 : Right 1 : Right 2 : Right 3 : Right 4 : nil)

        (illFormedList # InfList.takeWhileLazy (_ < 5))
          `shouldEqual` (Left ApplicationHung : nil)


    describe "find" do
      it "doesn't hang" do
        (wellFormedList # InfList.find (_ == 10))
          `shouldEqual` (Right 10)

        (illFormedList # InfList.find (_ == 10))
          `shouldSatisfy` isLeft 


    describe "index" do
      it "doesn't hang" do
        (wellFormedList # InfList.index 100) `shouldEqual` Right 100
        (illFormedList # InfList.index 100) `shouldSatisfy` isLeft

    describe "working with lists of maybes" do
      let 
        even i 
          | i `mod` 2 == 0 = Just i
          | otherwise = Nothing

      describe "mapMaybe" do
        it "doesn't hang" do
          (wellFormedList # InfList.mapMaybe even # InfList.take 3) `shouldEqual` Right (0:2:4:nil)
          (illFormedList # InfList.mapMaybe even # InfList.take 3) `shouldSatisfy` isLeft

      describe "catMaybes" do 
        it "doesn't hang" do
          (wellFormedList <#> even # InfList.catMaybes # InfList.take 3) `shouldEqual` Right (0:2:4:nil)
          (illFormedList <#> even # InfList.catMaybes # InfList.take 3) `shouldSatisfy` isLeft

    describe "chunkBySize" do
      it "doesn't hang" do
        (wellFormedList # InfList.chunkBySize 2 # InfList.take 3) 
          `shouldEqual` Right ((0:1:nil) : (2:3:nil) : (4:5:nil) : nil)

        (illFormedList # InfList.chunkBySize 2 # InfList.take 3)
          `shouldSatisfy` isLeft

        let listWith5 = InfList.iterate (_ + 1) (-4) {maxElements: 1000} # InfList.filter (_ <= 0)
        (listWith5 # InfList.chunkBySize 2 # InfList.take 2) 
          `shouldEqual` Right (((-4):(-3):nil) : ((-2):(-1):nil) : nil)

        (listWith5 # InfList.chunkBySize 2 # InfList.take 3) `shouldSatisfy` isLeft 

    describe "drop" do
      it "doesn't hang" do
        (wellFormedList # InfList.drop 5 # InfList.take 5)
          `shouldEqual` (Right (5:6:7:8:9:nil))

        (illFormedList # InfList.drop 5 # InfList.take 5) 
          `shouldSatisfy` isLeft

    describe "dropWhile" do
      it "doesn't hang" do
        (wellFormedList # InfList.dropWhile (_ < 10) # InfList.take 5)
          `shouldEqual` (Right (10:11:12:13:14:nil))

        (illFormedList # InfList.dropWhile (_ < 10) # InfList.take 5)
          `shouldSatisfy` isLeft

        (wellFormedList # InfList.dropWhile (const true) # InfList.take 1)
          `shouldSatisfy` isLeft

    describe "head" do
      it "doesn't hang" do
        (InfList.head wellFormedList) `shouldEqual` Right 0
        (InfList.head illFormedList) `shouldSatisfy` isLeft

    describe "uncons" do
      it "doesn't hang" do
        (InfList.uncons wellFormedList) `shouldSatisfy` 
          (case _ of
            Right { head, tail } -> head == 0 && InfList.take 3 tail == Right (1:2:3:nil)
            Left _ -> false)

        (InfList.uncons illFormedList) `shouldSatisfy` isLeft

        (InfList.uncons $ InfList.filter (_ == 0) wellFormedList) `shouldSatisfy` isRight

    describe "mapWithIndex" do
      it "doesn't hang" do
        (wellFormedList # InfList.mapWithIndex (\i x -> i + x) # InfList.take 5) 
          `shouldEqual` (Right (0:2:4:6:8:nil))

        (illFormedList # InfList.mapWithIndex (\i x -> i + x) # InfList.take 5)
          `shouldSatisfy` isLeft

    describe "zipWith" do
      it "doesn't hang" do
        (InfList.zipWith (+) wellFormedList wellFormedList # InfList.take 5)
          `shouldEqual` Right (0:2:4:6:8:nil)

        (InfList.zipWith (+) wellFormedList illFormedList # InfList.take 5)
          `shouldSatisfy` isLeft

        (InfList.zipWith (+) illFormedList wellFormedList # InfList.take 5)
          `shouldSatisfy` isLeft

    describe "zipWith finite + infinite" do
      it "doesn't hang" do
        (InfList.zipWithL (+) wellFormedList [1,2,3])
          `shouldEqual` Right [1,3,5]

        (InfList.zipWithL (+) illFormedList [1,2,3])
          `shouldSatisfy` isLeft

        (InfList.zipWithR (+) [1,2,3] wellFormedList) 
          `shouldEqual` Right [1,3,5]

        (InfList.zipWithR (+) [1,2,3] illFormedList)
          `shouldSatisfy` isLeft

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  infListSpecs