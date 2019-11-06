module Plot (component, PlotSeries) where

import React.Basic.Hooks (ReactComponent)

type PlotSeries = 
  { x :: Array Number
  , y :: Array Number
  , type :: String
  , mode :: String
  , marker :: { color :: String }
  }

foreign import component
  :: ReactComponent
      { data :: Array PlotSeries
      , layout :: { width :: Int, height :: Int, title :: String }
      }