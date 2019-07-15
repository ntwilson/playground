module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import React (fragmentWithKey)
import React as React
import ReactDOM as ReactDOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

import RefreshingPlot (refreshingPlot)

multiPlot :: React.ReactClass {} 
multiPlot = React.component "MultiPlot" \this ->
  pure 
    { state: {}
    , render: 
        pure $ fragmentWithKey "TwoPlots" 
          [ React.createLeafElement refreshingPlot {}
          , React.createLeafElement refreshingPlot {} ] 
    }

main :: Effect Unit
main = void $ do
  window <- DOM.window

  document <- DOM.document window

  let node = DOM.toNonElementParentNode document

  element <- DOM.getElementById "example" node

  let element' = unsafePartial (fromJust element)

  ReactDOM.render (React.createLeafElement multiPlot {}) element'

