module Lib (optimalSolutions) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

--     o
--     |
--     o
--     |
--     o
--    / \
--   o   o
--  /     \
-- o       o        

-- rebels must move each turn to an adjacent planet
-- police may move anywhere each turn, or stay at the same planet
-- rebels are caught if the police are at the same planet at the same time
-- objective is to find the shortest police route that guarantees the rebels are captured, 
-- regardless of their movements
-- police may enter the system at any planet on their first turn

data Planet = Outer1 | Outer2 | Outer3 | Inner1 | Inner2 | Inner3 | Center
  deriving (Eq, Ord, Show)

type PoliceMoves = [Planet]
type RebelPossibleLocations = Set Planet

type MoveSet = (PoliceMoves, RebelPossibleLocations)

possibleRebelMoves :: Planet -> Set Planet
possibleRebelMoves Outer1 = Set.fromList [Inner1]
possibleRebelMoves Outer2 = Set.fromList [Inner2]
possibleRebelMoves Outer3 = Set.fromList [Inner3]
possibleRebelMoves Inner1 = Set.fromList [Outer1, Center]
possibleRebelMoves Inner2 = Set.fromList [Outer2, Center]
possibleRebelMoves Inner3 = Set.fromList [Outer3, Center]
possibleRebelMoves Center = Set.fromList [Inner1, Inner2, Inner3]

allLocations :: Set Planet
allLocations = Set.fromList [ Outer1, Outer2, Outer3, Inner1, Inner2, Inner3, Center ] 

setConcat :: Ord a => Set (Set a) -> Set a
setConcat sets = 
  foldl Set.union Set.empty sets

setBind :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
setBind f x = 
  setConcat $ Set.map f x

(==<<) :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
f ==<< x = setBind f x

nextPossibleRebelLocations :: Set Planet -> Planet -> Set Planet
nextPossibleRebelLocations currentRebelLocations nextPoliceLocation =
  let totalRebelLocations = possibleRebelMoves ==<< currentRebelLocations
  in Set.delete nextPoliceLocation totalRebelLocations

startingMoveSets :: Set MoveSet
startingMoveSets = Set.fromList [([], allLocations)]

advanceOneTurn :: MoveSet -> Set MoveSet
advanceOneTurn (policeMoves, rebelPositions) = 
  let 
    advanceToLocation nextPoliceLocation = 
      (nextPoliceLocation:policeMoves, nextPossibleRebelLocations rebelPositions nextPoliceLocation)
  in Set.map advanceToLocation allLocations

numRebelPossibilies :: MoveSet -> Int
numRebelPossibilies (_policeMoves, rebelPositions) = 
  Set.size rebelPositions

advanceAndTrim :: Set MoveSet -> Set MoveSet
advanceAndTrim moveSets = 
  let 
    allMoveSets = advanceOneTurn ==<< moveSets
    minRebelPossibilities = Set.lookupMin $ Set.map numRebelPossibilies allMoveSets
  in 
    case minRebelPossibilities of
      Just minRebels -> Set.filter (\moveSet -> numRebelPossibilies moveSet == minRebels) allMoveSets
      Nothing -> moveSets

setElemExists :: (a -> Bool) -> Set a -> Bool
setElemExists predicate xs = 
  case List.find predicate $ Set.toList xs of
    Just _  -> True
    Nothing -> False


optimalSolutions :: Set PoliceMoves
optimalSolutions = 
  let 
    infiniteTurns = List.iterate advanceAndTrim startingMoveSets
    winningTurn = List.find (setElemExists (\moveSet -> numRebelPossibilies moveSet == 0)) x
  in 
    case winningTurn of
      Just movesThatWinThisTurn -> Set.map (\(policeMoves, _rebelPositions) -> policeMoves) movesThatWinThisTurn
      Nothing -> error "reached the end of an infinite list???"

