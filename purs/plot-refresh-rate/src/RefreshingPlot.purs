module RefreshingPlot (refreshingPlot) where

import Prelude

import Plot as Plot
import React as React
import React.DOM as React
import RefreshingPlot.Pure (initialState, mkYData, refreshingPlotReducer)
import Timer (nowMS, setTimeout)

refreshingPlot :: React.ReactClass { }
refreshingPlot = 
  React.component "RefreshingPlot" \this -> do
    state <- initialState
    pure 
      { state
      , getSnapshotBeforeUpdate: \_props _state -> nowMS
      , componentDidUpdate: \_prevProps prevState timeBeforeRender -> do
          timeAfterRender <- nowMS

          newYData <- mkYData

          let newState = refreshingPlotReducer timeBeforeRender timeAfterRender (prevState { yData = newYData })

          flip setTimeout 0 $ React.setState this newState

      , componentDidMount: do
          oldState <- React.getState this
          newYData <- mkYData
          React.setState this $ oldState { yData = newYData }

      , render: do
          { xData, yData, avgRefreshRate, avgRedrawDuration } <- React.getState this
          pure $ 
            React.fragmentWithKey "Form" 
              [ React.div' [React.toElement "avg time to draw graph on the page (ms):"]
              , React.div' [React.toElement $ show avgRedrawDuration]
              , React.div' [React.toElement $ "refresh rate (Hz):"]
              , React.div' [React.toElement $ show avgRefreshRate]
              , React.createLeafElement Plot.component
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
      }
