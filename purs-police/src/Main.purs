module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Exception.Unsafe (unsafeThrow)

import Data.Foldable (foldl)
import Data.Set (Set)
import Data.Set as Set
import Data.List (List)
import Data.List as List
import Data.List.Infinite as IList
import Data.Maybe (Maybe(..))
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
derive instance eqPlanet :: Eq Planet
derive instance ordPlanet :: Ord Planet
instance showPlanet :: Show Planet where
  show Outer1 = "Outer1"
  show Outer2 = "Outer2"
  show Outer3 = "Outer3"
  show Inner1 = "Inner1"
  show Inner2 = "Inner2"
  show Inner3 = "Inner3"
  show Center = "Center"

type PoliceMoves = List Planet
type RebelPossibleLocations = Set Planet

type MoveSet = { police :: PoliceMoves, rebels :: RebelPossibleLocations }

possibleRebelMoves :: Planet -> Set Planet
possibleRebelMoves Outer1 = Set.fromFoldable [Inner1]
possibleRebelMoves Outer2 = Set.fromFoldable [Inner2]
possibleRebelMoves Outer3 = Set.fromFoldable [Inner3]
possibleRebelMoves Inner1 = Set.fromFoldable [Outer1, Center]
possibleRebelMoves Inner2 = Set.fromFoldable [Outer2, Center]
possibleRebelMoves Inner3 = Set.fromFoldable [Outer3, Center]
possibleRebelMoves Center = Set.fromFoldable [Inner1, Inner2, Inner3]

allLocations :: Set Planet
allLocations = Set.fromFoldable [ Outer1, Outer2, Outer3, Inner1, Inner2, Inner3, Center ] 

setConcat :: forall a. Ord a => Set (Set a) -> Set a
setConcat sets = 
  foldl Set.union Set.empty sets

setBind :: forall a b. Ord a => Ord b => (a -> Set b) -> Set a -> Set b
setBind f x = 
  setConcat $ Set.map f x

infixr 5 setBind as ==<<

nextPossibleRebelLocations :: Set Planet -> Planet -> Set Planet
nextPossibleRebelLocations currentRebelLocations nextPoliceLocation =
  let totalRebelLocations = possibleRebelMoves ==<< currentRebelLocations
  in Set.delete nextPoliceLocation totalRebelLocations

startingMoveSets :: Set MoveSet
startingMoveSets = Set.fromFoldable $ List.singleton { police: List.Nil, rebels: allLocations }

advanceOneTurn :: MoveSet -> Set MoveSet
advanceOneTurn { police, rebels } = 
  let 
    advanceToLocation nextPoliceLocation = 
      { police: List.Cons nextPoliceLocation police, 
        rebels: nextPossibleRebelLocations rebels nextPoliceLocation }
  in Set.map advanceToLocation allLocations

numRebelPossibilies :: MoveSet -> Int
numRebelPossibilies { rebels } = 
  Set.size rebels

advanceAndTrim :: Set MoveSet -> Set MoveSet
advanceAndTrim moveSets = 
  let 
    allMoveSets = advanceOneTurn ==<< moveSets
    minRebelPossibilities = Set.findMin $ Set.map numRebelPossibilies allMoveSets
  in 
    case minRebelPossibilities of
      Just minRebels -> Set.filter (\moveSet -> numRebelPossibilies moveSet == minRebels) allMoveSets
      Nothing -> moveSets

setElemExists :: forall a. (a -> Boolean) -> Set a -> Boolean
setElemExists predicate xs = 
  case List.find predicate $ (Set.toUnfoldable xs :: List a) of
    Just _  -> true
    Nothing -> false


optimalSolutions :: Set PoliceMoves
optimalSolutions = 
  let 
    infiniteTurns = IList.iterate advanceAndTrim startingMoveSets
    winningTurn = IList.head $ IList.filter (setElemExists (\moveSet -> numRebelPossibilies moveSet == 0)) infiniteTurns
  in 
    Set.map (\{ police } -> police) winningTurn

optimalLength :: Int
optimalLength = 
  case Set.findMin optimalSolutions of
    Just aSolution -> List.length aSolution
    Nothing -> unsafeThrow "no solutions found"

main :: Effect Unit
main = do
  (logShow optimalSolutions :: Effect Unit)
  log ""
  log ("The optimal solution requires " <> (show optimalLength) <> " moves")
  log ((show $ Set.size optimalSolutions) <> " optimal solutions found")
