module Prelude (module Relude, (#), (<#>)) where

import Relude

(#) :: a -> (a -> b) -> b
infixl 5 #
x # f = f x

(<#>) :: Functor f => f a -> (a -> b) -> f b
infixl 5 <#>
x <#> f = f <$> x
