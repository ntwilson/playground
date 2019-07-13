module Timer (time, nowMS, setTimeout) where

import Prelude

import Effect (Effect)
import Data.Tuple (Tuple)

foreign import time :: forall a. 
  Effect a -> Effect (Tuple Number a)

foreign import nowMS :: Effect Number

foreign import setTimeout :: Effect Unit -> Int -> Effect Unit