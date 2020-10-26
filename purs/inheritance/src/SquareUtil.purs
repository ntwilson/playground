module SquareUtil (asSquare, squares) where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import TryStuff (SomeShape, Square, boxedAsShape, shapes)
import Unsafe.Coerce (unsafeCoerce)

foreign import isSquare :: SomeShape -> Boolean
asSquare :: SomeShape -> Maybe Square
asSquare x 
  | isSquare x = Just $ unsafeCoerce x
  | otherwise = Nothing

squares :: Array Square
squares = (boxedAsShape <$> shapes) # mapMaybe asSquare
