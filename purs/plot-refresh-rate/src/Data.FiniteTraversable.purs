module Data.FiniteTraversable where

import Data.List (List)
import Data.Traversable (class Traversable)

class Traversable a <= FiniteTraversable a

instance finiteList :: FiniteTraversable List

instance finiteArray :: FiniteTraversable Array
