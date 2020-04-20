module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import React.Basic.Hooks as React
import React.Basic.DOM as ReactDOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

import RefreshingPlot (refreshingPlot)

multiPlot :: Effect (React.ReactComponent {})
multiPlot = do
  refresh <- refreshingPlot
  React.component "MultiPlot" \props ->
    pure $ React.fragment
      [ React.element refresh {}
      , React.element refresh {} ] 

main :: Effect Unit
main = void $ do
  window <- DOM.window

  document <- DOM.document window

  let node = DOM.toNonElementParentNode document

  element <- DOM.getElementById "example" node

  let element' = unsafePartial (fromJust element)

  plot <- multiPlot

  ReactDOM.render (React.element plot {}) element'

