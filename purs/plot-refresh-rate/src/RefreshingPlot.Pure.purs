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
  , durationsFromInstants
  , refreshRate
  , refreshingPlotReducer
  , Refresh (..)
  , initialState
  , mkYData) where


import Prelude

import Data.Array ((..))
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List, (:))
import Data.List (List(..), length, take) as List
import Data.List.Extensions (pairwise) as List
import Data.Traversable (sequence)
import Data.Tuple.Nested((/\))
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
  , last10RefreshTimes :: List Time
  , avgRefreshRate :: Number }

initialState :: Effect RefreshingPlotState
initialState = do
  yData <- mkYData
  pure 
    { xData: 1 .. 500 <#> Int.toNumber
    , yData
    , last10RefreshTimes: List.Nil
    , avgRefreshRate: 0.0 }

durationsFromInstants :: List Time -> List Duration
durationsFromInstants instants = do
  later /\ earlier <- List.pairwise instants 
  pure $ later - earlier

refreshRate :: List Duration -> Number
refreshRate last10RefreshDurations =
  let 
    nDurations = List.length last10RefreshDurations
    averageDuration = sum last10RefreshDurations / Int.toNumber nDurations
  in
    if nDurations == 0 then 0.0
    else 1000.0 / averageDuration

data Refresh = Refresh Time (Array Number)

refreshingPlotReducer :: RefreshingPlotState -> Refresh -> RefreshingPlotState
refreshingPlotReducer (oldState@{last10RefreshTimes}) (Refresh timeAfterRender newYData) = 
  let 
    newRefreshTimes = List.take 10 $ timeAfterRender : last10RefreshTimes
    last10RefreshDurations = durationsFromInstants newRefreshTimes

    avgRefreshRate = refreshRate last10RefreshDurations

  in oldState 
    { avgRefreshRate = avgRefreshRate
    , last10RefreshTimes = newRefreshTimes
    , yData = newYData }

