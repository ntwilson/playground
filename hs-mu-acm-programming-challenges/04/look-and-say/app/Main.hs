module Main where

import Lib (lookAndSayAndSum)
import qualified Either

text :: String -> Text
text = toText

main :: IO ()
main = do
  putTextLn (text "Please enter a line with however many cases to run, followed by a line per case with a single integer")
  putTextLn (text "")

  n <- (readEither <$> getLine) :: IO (Either Text Int)
  nCases <- n # Either.unless (text "each run must begin with a line with a single integer for the number of cases")

  cases <- sequence ([1 .. nCases] <#> const getLine)

  putTextLn (text "")
  forM_ cases $ \input ->
    case lookAndSayAndSum $ toString input of
      Right x -> putTextLn x
      Left _ -> fail "inputs must be integers.  Every character of the input must be numeric"

