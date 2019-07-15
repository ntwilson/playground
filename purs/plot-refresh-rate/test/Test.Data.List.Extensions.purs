module Test.Data.List.Extensions (listSpecs) where

import Prelude

import Data.Identity (Identity)
import Data.List ((..), (:), List(..))
import Data.List as List
import Data.List.Extensions (pairwise)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)

listSpecs :: SpecT Aff Unit Identity Unit
listSpecs = do
  describe "Data.List.Extensions-spec" do
    describe "pairwise" do
      it "gives all consecutive pairs of a list" do
        let init = 1..5
        pairwise init `shouldEqual`
          (Tuple 1 2 : Tuple 2 3 : Tuple 3 4 : Tuple 4 5 : Nil)

      it "returns an empty list if there isn't enough for a single pair" do
        let init = List.singleton 1
        pairwise init `shouldEqual` Nil
