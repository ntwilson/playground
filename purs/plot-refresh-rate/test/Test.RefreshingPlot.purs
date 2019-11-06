module Test.RefreshingPlot (refreshingPlotSpecs) where

import Prelude

import Data.Identity (Identity)
import Data.Int as Int
import Data.List (List(..), (..), (:))
import Data.List as List
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import RefreshingPlot.Pure (Refresh(..), durationsFromInstants, initialState, mkYData, refreshRate, refreshingPlotReducer)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

refreshingPlotSpecs :: SpecT Aff Unit Identity Unit
refreshingPlotSpecs = do
  describe "refreshingPlot" do
    describe "durationsFromInstants" do
      it "calculates the difference of adjacent elements" do
        let 
          times = 5.0 : 4.75 : 4.0 : 3.5 : Nil

        durationsFromInstants times `shouldEqual` (0.25 : 0.75 : 0.5 : Nil)

      it "returns an empty list if there is only one time to work with" do
        let times = 5.0 : Nil
        durationsFromInstants times `shouldEqual` Nil

    describe "refreshRate" do
      it "returns 0 if there are no durations" do
        let durations = Nil
        refreshRate durations `shouldEqual` 0.0

      it "returns the average frequency in Hz given a set of durations" do
        let durations = 95 .. 105 <#> Int.toNumber -- average duration: 100
        refreshRate durations `shouldEqual` 10.0

    describe "refreshingPlotReducer" do
      it "produces the average refresh rate and average redraw time" do
        state <- liftEffect initialState
        yData <- liftEffect mkYData
        let  
          stateWithData = 
            state 
              { last10RefreshTimes = 500.0 : 450.0 : 425.0 : 350.0 : 300.0 : 275.0 : Nil }

          timeAfterRender = 575.0

          newState = refreshingPlotReducer stateWithData $ Refresh timeAfterRender yData

        newState.last10RefreshTimes `shouldEqual` (575.0 : 500.0 : 450.0 : 425.0 : 350.0 : 300.0 : 275.0 : Nil)

        newState.avgRefreshRate `shouldEqual` 20.0 -- 75;50;25;75;50;25 avgs 50 ms = 20Hz

      it "doesn't choke on the initial state" do
        state <- liftEffect initialState
        yData <- liftEffect mkYData
        let 
          timeAfterRender = 1.0
          newState = refreshingPlotReducer state $ Refresh timeAfterRender yData

        newState.avgRefreshRate `shouldEqual` 0.0

        newState.last10RefreshTimes `shouldEqual` (1.0 : Nil)
          

