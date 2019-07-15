-- Contains all of the testable logic of the RefreshingPlot.
-- Normally this would be defined in the same file as the component,
-- but the Plot.ly lib cannot be imported into any project that runs
-- tests, since just by importing the library, it tries to access the 
-- global `document`, which isn't defined in node.js (the platform we 
-- run tests on).

module RefreshingPlot.Pure
  ( Time
  , Duration
  , RefreshingPlotState
  , redrawDurations
  , durationsFromInstants
  , refreshRate
  , refreshingPlotReducer
  , initialState
  , mkYData) where


import Prelude

import Data.Array ((..))
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.List.Extensions as List
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (random)


mkDataPoint :: Effect Number
mkDataPoint = do
  x <- random
  pure $ 500.0 + (200.0 * x)

mkYData :: Effect (Array Number)
mkYData = sequence (1 .. 500 <#> const mkDataPoint)

type Time = Number
type Duration = Number 

type RefreshingPlotState = 
  { xData :: Array Number
  , yData :: Array Number
  , last10RedrawDurations :: List Duration
  , last10RefreshTimes :: List Time
  , avgRedrawDuration :: Duration
  , avgRefreshRate :: Number }

initialState :: Effect RefreshingPlotState
initialState = do
  yData <- mkYData
  pure 
    { xData: 1 .. 500 <#> Int.toNumber
    , yData
    , last10RedrawDurations: List.Nil
    , last10RefreshTimes: List.Nil
    , avgRedrawDuration: 0.0
    , avgRefreshRate: 0.0 }

redrawDurations :: Time -> Time -> List Duration -> { newDurations :: List Duration, avgDuration :: Duration }
redrawDurations timeBeforeRender timeAfterRender last10RedrawDurations = 
  let 
    timeToRedrawInMS = timeAfterRender - timeBeforeRender
    redraws = List.take 10 $ timeToRedrawInMS:last10RedrawDurations
    avgRedrawDuration = sum redraws / Int.toNumber (List.length redraws)
  in
    { newDurations: redraws, avgDuration: avgRedrawDuration }

durationsFromInstants :: List Time -> List Duration
durationsFromInstants instants = do
  Tuple later earlier <- List.pairwise instants 
  pure $ later - earlier

refreshRate :: List Duration -> Number
refreshRate last10RefreshDurations =
  let 
    nDurations = List.length last10RefreshDurations
    averageDuration = sum last10RefreshDurations / Int.toNumber nDurations
  in
    if nDurations == 0 then 0.0
    else 1000.0 / averageDuration


refreshingPlotReducer :: Time -> Time -> RefreshingPlotState -> RefreshingPlotState
refreshingPlotReducer timeBeforeRender timeAfterRender (oldState@{last10RedrawDurations, last10RefreshTimes}) = 
  let 
    { newDurations: redraws, avgDuration: avgRedrawDuration } = 
      redrawDurations timeBeforeRender timeAfterRender last10RedrawDurations

    newRefreshTimes = List.take 10 $ timeAfterRender : last10RefreshTimes
    last10RefreshDurations = durationsFromInstants newRefreshTimes

    avgRefreshRate = refreshRate last10RefreshDurations

  in oldState 
    { avgRedrawDuration = avgRedrawDuration
    , avgRefreshRate = avgRefreshRate
    , last10RedrawDurations = redraws
    , last10RefreshTimes = newRefreshTimes }

