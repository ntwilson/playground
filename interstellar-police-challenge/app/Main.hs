module Main where

import Prelude ()
import Relude

import Lib (optimalSolutions)
import System.TimeIt (timeIt)
import qualified Data.Set as Set 
import qualified Data.List as List

printSolutions :: IO () 
printSolutions = do
  print optimalSolutions
  putStrLn ""
  putStrLn ("The optimal solution requires " ++ (show $ List.length $ Set.findMin optimalSolutions) ++ " moves")
  putStrLn ((show $ Set.size optimalSolutions) ++ " optimal solutions found")

main :: IO ()
main = timeIt printSolutions
