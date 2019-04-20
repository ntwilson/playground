module Maybe (Maybe.unless) where

unless :: Text -> Maybe a -> IO a
unless _ (Just x) = pure x
unless txt Nothing = fail $ toString txt
