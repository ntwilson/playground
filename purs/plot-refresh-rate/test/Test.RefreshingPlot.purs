module Test.RefreshingPlot (refreshingPlotSpecs) where

import Prelude

import Data.Identity (Identity)
import Data.Int as Int
import Data.List (List(..), (..), (:))
import Data.List as List
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import RefreshingPlot.Pure (durationsFromInstants, initialState, redrawDurations, refreshRate, refreshingPlotReducer)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

refreshingPlotSpecs :: SpecT Aff Unit Identity Unit
refreshingPlotSpecs = do
  describe "refreshingPlot" do
    describe "redrawDurations" do
      it "appends the new duration to the list" do
        let 
          initDurations = Nil
          timeBeforeRender = 3.5
          timeAfterRender = 3.75
          { newDurations } = redrawDurations timeBeforeRender timeAfterRender initDurations

        newDurations `shouldEqual` List.singleton 0.25
    
      it "averages the full set of durations" do
        let 
          initDurations = 1 .. 5 <#> Int.toNumber
          timeBeforeRender = 2.5
          timeAfterRender = 5.5
          { avgDuration } = redrawDurations timeBeforeRender timeAfterRender initDurations

        avgDuration `shouldEqual` 3.0

      it "cuts off after 10 durations" do
        let 
          initDurations = 1 .. 13 <#> Int.toNumber 
          timeBeforeRender = 2.0
          timeAfterRender = 12.0  -- new duration of 10.0; 1.0 .. 10.0 averages to 5.5
          { newDurations, avgDuration } = redrawDurations timeBeforeRender timeAfterRender initDurations

        newDurations `shouldEqual` (10.0 : (1 .. 9 <#> Int.toNumber))
        avgDuration `shouldEqual` 5.5

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
        let  
          stateWithData = 
            state 
              { last10RedrawDurations = 60.0 : 70.0 : 80.0 : 90.0 : Nil
              , last10RefreshTimes = 500.0 : 450.0 : 425.0 : 350.0 : 300.0 : 275.0 : Nil }

          timeBeforeRender = 525.0
          timeAfterRender = 575.0

          newState = refreshingPlotReducer timeBeforeRender timeAfterRender stateWithData

        newState.last10RedrawDurations `shouldEqual` (50.0 : 60.0 : 70.0 : 80.0 : 90.0 : Nil)
        newState.last10RefreshTimes `shouldEqual` (575.0 : 500.0 : 450.0 : 425.0 : 350.0 : 300.0 : 275.0 : Nil)

        newState.avgRedrawDuration `shouldEqual` 70.0
        newState.avgRefreshRate `shouldEqual` 20.0 -- 75;50;25;75;50;25 avgs 50 ms = 20Hz

      it "doesn't choke on the initial state" do
        state <- liftEffect initialState
        let 
          timeBeforeRender = 0.5
          timeAfterRender = 1.0
          newState = refreshingPlotReducer timeBeforeRender timeAfterRender state

        newState.avgRefreshRate `shouldEqual` 0.0
        newState.avgRedrawDuration `shouldEqual` 0.5

        newState.last10RefreshTimes `shouldEqual` (1.0 : Nil)
        newState.last10RedrawDurations `shouldEqual` (0.5 : Nil)
          

