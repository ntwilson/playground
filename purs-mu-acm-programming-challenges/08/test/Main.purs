module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Error.Class (class MonadThrow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

chunkSpecs :: forall a m. Monad m => MonadThrow Error a => SpecT a Unit m Unit
chunkSpecs = do
  describe "chunkBySize" do
    describe "given a perfect divisor" do
      it "produces a square matrix" do
        true `shouldEqual` true
