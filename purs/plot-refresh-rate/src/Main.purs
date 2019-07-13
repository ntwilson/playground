module Main where

import Prelude

import Data.Array ((..))
import Data.Foldable (sum)
import Data.Int as Int
import Data.List ((:))
import Data.List as List
import Data.Maybe (fromJust, Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (random)
import Partial.Unsafe (unsafePartial)
import Plot as Plot
import React (fragmentWithKey)
import React as React
import React.DOM as React
import ReactDOM as ReactDOM
import Timer (nowMS, setTimeout)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

refreshingPlot :: React.ReactClass { }
refreshingPlot = 
  React.component "RefreshingPlot" \this -> do
    let xData = 1..500 <#> Int.toNumber
    yData <- sequence (1..500 <#> const dataPoint)

    pure 
      { state: 
        { xData
        , yData
        , last10RedrawDurations: List.Nil
        , last10RefreshDurations: List.Nil
        , lastRefreshTime: Nothing
        , redrawDuration: 0.0
        , refreshRate: 0.0 }
      , getSnapshotBeforeUpdate: \props state -> nowMS
      , componentDidUpdate: \prevProps (prevState@{ last10RedrawDurations, last10RefreshDurations, lastRefreshTime }) timeBeforeRender -> do
          timeAfterRender <- nowMS
          let 
            timeToRedrawInMS = timeAfterRender - timeBeforeRender
            timeToRefreshInMS = 
              case lastRefreshTime of
                Just time -> Just $ timeAfterRender - time
                Nothing -> Nothing
            redraws = 
              case last10RedrawDurations of
                a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:_ -> timeToRedrawInMS:a1:a2:a3:a4:a5:a6:a7:a8:a9:List.Nil
                _ -> timeToRedrawInMS : last10RedrawDurations

            averageRedrawDuration = sum redraws / Int.toNumber (List.length redraws)

            Tuple refreshRate refreshes = 
              case Tuple timeToRefreshInMS last10RefreshDurations of
                Tuple Nothing _ -> 
                  Tuple 0.0 List.Nil
                Tuple (Just refreshDuration) (a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:_) -> 
                  let 
                    durations = refreshDuration:a1:a2:a3:a4:a5:a6:a7:a8:a9:List.Nil
                    averageDuration = sum durations / 10.0
                  in Tuple (1000.0 / averageDuration) durations 
                Tuple (Just refreshDuration) _ -> 
                  let 
                    durations = refreshDuration : last10RefreshDurations
                    averageDuration = sum durations / Int.toNumber (List.length durations)
                  in Tuple (1000.0 / averageDuration) durations

          flip setTimeout 0 $ do
            newYData <- sequence (1..500 <#> const dataPoint)
            React.setState this 
              { xData
              , yData: newYData
              , last10RedrawDurations: redraws
              , last10RefreshDurations: refreshes
              , lastRefreshTime: Just timeAfterRender
              , redrawDuration: averageRedrawDuration
              , refreshRate } 
      , componentDidMount: do
          newYData <- sequence (1..500 <#> const dataPoint) 
          React.setState this { xData, yData: newYData }
      , render: do
          { yData: newYData, refreshRate, redrawDuration } <- React.getState this
          pure $ 
            React.fragmentWithKey "Form" 
              [ React.div' [React.toElement "avg time to draw graph on the page (ms):"]
              , React.div' [React.toElement $ show redrawDuration]
              , React.div' [React.toElement $ "refresh rate (Hz):"]
              , React.div' [React.toElement $ show refreshRate]
              , React.createLeafElement Plot.component
                  { data: 
                      [
                        { x: xData
                        , y: newYData
                        , type: "scatter"
                        , mode: "lines"
                        , marker: { color: "red" } 
                        }
                      ] 
                  , layout: { width: 640, height: 440, title: "test plot" }
                  }
              ]
      }

  where
    dataPoint = do
      x <- random
      pure $ 500.0 + (200.0 * x)


multiPlot :: React.ReactClass {} 
multiPlot = React.component "MultiPlot" \this ->
  pure 
    { state: {}
    , render: 
        pure $ fragmentWithKey "TwoPlots" 
          [ React.createLeafElement refreshingPlot {}
          , React.createLeafElement refreshingPlot {}
          ]
    }

main :: Effect Unit
main = void $ do
  window <- DOM.window

  document <- DOM.document window

  let node = DOM.toNonElementParentNode document

  element <- DOM.getElementById "example" node

  let element' = unsafePartial (fromJust element)

  ReactDOM.render (React.createLeafElement multiPlot {}) element'

