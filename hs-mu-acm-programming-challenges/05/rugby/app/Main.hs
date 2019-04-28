{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib (countCombos)
import qualified Either

main :: IO ()
main = do
  putTextLn "Please enter a line with however many cases to run, followed by a line per case with a single integer"
  putTextLn ""

  n <- (readEither <$> getLine)
  let 
    nCases = n # Either.unless "each run must begin with a line with a single integer for the number of cases"

  cases <- sequence ([1 .. nCases] <#> const loadCase)
  putTextLn ""

  let answers = show . countCombos <$> cases
  
  forM_ answers putTextLn 

  where

    loadCase = do
      x <- getLine
      pure $ readEither x # Either.unless "inputs must be integers.  Every character of the input must be numeric"
