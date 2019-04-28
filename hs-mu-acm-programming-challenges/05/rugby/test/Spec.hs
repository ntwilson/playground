import Test.HUnit
import Lib (Score (..), pointsOf, countCombos, combos) --, (+++))
import Text.Printf (printf)
import Test.QuickCheck


main :: IO ()
main = do
  _ <- 
    runTestTT (TestList [
      TestCase $ assertBool "should assign proper point values to each type of score"
        ( 
          pointsOf Try == 4
          && 
          pointsOf DropGoal == 1
        )
      ,
      TestCase $ do 
        let [of0,of1,of2,of3,of4,of5,of6] = countCombos <$> [0 .. 6]

        assertEqual (printf "countCombos 0 (%i) = 1" of0) of0 1
        assertEqual (printf "countCombos 1 (%i) = 1" of1) of1 1
        assertEqual (printf "countCombos 2 (%i) = 3" of2) of2 3
        assertEqual (printf "countCombos 3 (%i) = 3" of3) of3 3
        assertEqual (printf "countCombos 4 (%i) = 7" of4) of4 7
        assertEqual (printf "countCombos 5 (%i) = 7" of5) of5 7
        assertEqual (printf "countCombos 6 (%i) = 13" of6) of6 13
    ] )

  quickCheck prop_eachSolutionIsTheRightNumberOfPoints


prop_eachSolutionIsTheRightNumberOfPoints :: Property
prop_eachSolutionIsTheRightNumberOfPoints = 
  forAll 
    (elements [0 .. 20]) $ 
    \i -> combos i # all (\solution -> sum (pointsOf <$> solution) == i)
