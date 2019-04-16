module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.List as List
import Data.List.Lazy as LList
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Exception.Unsafe (unsafeThrow)
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
derive instance genericPlanet :: Generic Planet _
instance showPlanet :: Show Planet where show = genericShow

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

setBind :: forall a b. Ord a => Ord b => (a -> Set b) -> Set a -> Set b
setBind f x = 
  Set.unions $ Set.map f x

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
    infiniteTurns = LList.iterate advanceAndTrim startingMoveSets
    winningTurn = LList.head $ LList.filter (setElemExists (\moveSet -> numRebelPossibilies moveSet == 0)) infiniteTurns
  in 
    case winningTurn of
      Just optimalMoves -> Set.map (\{ police } -> police) optimalMoves
      Nothing -> unsafeThrow "reached the end of an infinite list???"

optimalLength :: Int
optimalLength = 
  case Set.findMin optimalSolutions of
    Just aSolution -> List.length aSolution
    Nothing -> unsafeThrow "no solutions found"

main :: Effect Unit
main = do
  let formattedSolutions = (Set.toUnfoldable $ Set.map List.toUnfoldable optimalSolutions) :: Array (Array Planet) 
  logShow formattedSolutions
  log ""
  log ("The optimal solution requires " <> (show optimalLength) <> " moves")
  log ((show $ Set.size optimalSolutions) <> " optimal solutions found")
