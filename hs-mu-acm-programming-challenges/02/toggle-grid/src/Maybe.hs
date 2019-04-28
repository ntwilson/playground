module Maybe (Maybe.unless) where

import Control.Exception (throw)
import Exceptions (BadAssumption (..))

unless :: Text -> Maybe a -> a
unless _ (Just x) = x
unless txt Nothing = throw $ BadAssumption txt
