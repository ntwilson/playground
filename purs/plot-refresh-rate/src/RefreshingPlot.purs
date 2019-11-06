module RefreshingPlot (refreshingPlot) where

import Prelude

import Effect (Effect)
import Plot as Plot
import React.Basic.DOM as React
import React.Basic.Hooks (ReactComponent, component, readRef, (/\))
import React.Basic.Hooks as React
import RefreshingPlot.Pure (initialState, mkYData, refreshingPlotReducer, Refresh(..))
import Timer (nowMS, setTimeout)

refreshingPlot :: Effect (ReactComponent { })
refreshingPlot = do
  init <- initialState
  currentTime <- nowMS
  component "RefreshingPlot" \this -> React.do
    state /\ dispatch <- React.useReducer init refreshingPlotReducer

    React.useEffect state do
      currTime <- nowMS
      newYData <- mkYData
      flip setTimeout 0 $ dispatch $ Refresh currTime newYData
      pure $ pure unit

    let { xData, yData, avgRefreshRate } = state

    pure $ React.fragment 
      [ React.div { children: [React.text $ "refresh rate (Hz):"] }
      , React.div { children: [React.text $ show avgRefreshRate] }
      , React.element Plot.component
          { data: 
              [
                { x: xData
                , y: yData
                , type: "scatter"
                , mode: "lines"
                , marker: { color: "red" } 
                }
              ] 
          , layout: { width: 640, height: 440, title: "test plot" } }
      ]
