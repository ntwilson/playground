{-# LANGUAGE OverloadedStrings #-}
module Lib (Score (..), pointsOf, countCombos, (+++), combos) where

import qualified Data.Set as Set

data Score = Try | GoalKick | Penalty | DropGoal deriving (Show, Ord, Eq)
pointsOf :: Score -> Int
pointsOf Try = 4
pointsOf GoalKick = 2
pointsOf Penalty = 2
pointsOf DropGoal = 1

distinct :: Ord a => [a] -> [a] 
distinct = toList . Set.fromList


{-
  so the thought is that this can be recursive, if we have a way to combine sets.  e.g., the 
  combos that would work for 6 = the combos for 4 combined with the combos for 2.  I'm using a 
  (*) operator for combining sets below, because it's closely related to a cartesian product

  1: DG = 1
  2: GK|P|DG+DG = 3
  3: f 2 * f 1 = 3
  4: Try | f 2 * f 2 = 7
  5: f 4 * f 1 = 7
  6: f 4 * f 2 = T|GK+GK|GK+P|GK+DG+DG|P+P|P+DG+DG|4xDG * GK|P|DG+DG
           = T+GK|T+P|T+DG+DG | GK+GK+GK|GK+GK+P|GK+GK+DG+DG
           | GK+P+P|GK+P+DG+DG | GK+4xDG
           | P+P+P|P+P+DG+DG | P+4xDG | 6xDG
           = 13

           = T+(everything) | GK+GK+(everything but T+_) | GK+P+(everything but T+_|GK+_) | GK+DG+DG+(everything but T+_|GK+_|P+_)
           | P+P+(everything but T+_|GK+_) | P+DG+DG+(everything but T+_|GK+_|P+_) | 4xDG+(everything but T+_|GK+_|P+_)

  interesting idea, but I'm getting lost in how to implement that.  I'm brute forcing it below by computing the cartesian product
  and then removing duplicates
-}

(+++) :: [[Score]] -> [[Score]] -> [[Score]]
a +++ b = distinct [ sort (x <> y) | x <- a, y <- b ]

combos :: Int -> [[Score]]
combos 0 = [[]]
combos 1 = [[DropGoal]]
combos 2 = [[GoalKick], [Penalty], [DropGoal, DropGoal] ]
combos 4 = [[Try]] <> (combos 2 +++ combos 2)
combos n 
  | n >= 4 = combos 4 +++ combos (n - 4)
  | n >= 2 = combos 2 +++ combos (n - 2)
  | otherwise = combos 1 +++ (combos (n - 1))

countCombos :: Int -> Int
countCombos finalScore = length $ combos finalScore
