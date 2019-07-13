module Plot (component, PlotSeries) where

import React (ReactClass)

type PlotSeries = 
  { x :: Array Number
  , y :: Array Number
  , type :: String
  , mode :: String
  , marker :: { color :: String }
  }

foreign import component
  :: ReactClass
      { data :: Array PlotSeries
      , layout :: { width :: Int, height :: Int, title :: String }
      }