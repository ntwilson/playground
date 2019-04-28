module Either (Either.unless, expect) where

import Control.Exception (throw)
import Exceptions (BadAssumption (..), ReportableException)

unless :: Text -> Either l a -> a
unless _ (Right x) = x
unless txt (Left _) = throw $ BadAssumption txt

expect :: ReportableException e => Either e r -> r
expect (Right x) = x
expect (Left e) = throw e 
