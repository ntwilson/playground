module Either (Either.unless, expect) where

unless :: Text -> Either l a -> IO a
unless _ (Right x) = pure x
unless txt (Left _) = fail $ toString txt

expect :: Either Text r -> IO r
expect (Right x) = pure x
expect (Left msg) = fail $ toString msg 
