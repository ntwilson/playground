module Data.List.Extensions (pairwise) where

import Data.List (List(..), (:), reverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

pairwise :: forall a. List a -> List (Tuple a a)
pairwise xs = go Nil xs
  where 
    go acc (x:y:rest) = go ((x /\ y) : acc) (y:rest)
    go acc _ = reverse acc

