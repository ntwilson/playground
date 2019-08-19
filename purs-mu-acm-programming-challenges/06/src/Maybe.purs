module Data.Maybe.FromJustUnless (fromJustUnless, unsafeFromJustUnless) where

import Data.Maybe (Maybe(..))
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

fromJustUnless :: forall a. Partial => String -> Maybe a -> a
fromJustUnless msg (Just x) = x
fromJustUnless msg Nothing = crashWith msg

unsafeFromJustUnless :: forall a. String -> Maybe a -> a
unsafeFromJustUnless msg x = unsafePartial (fromJustUnless msg x)
