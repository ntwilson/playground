{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
module Main where

import Lib
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Exceptions (ReportableException)
import qualified Either

text :: String -> Text
text = toText

initialGrid :: LightGrid
initialGrid = 
  let singleRow = [False | _ <- [1 .. 100]]
  in [ singleRow | _ <- [1 .. 100]]

data BadInstructionSet = BadInstructionSet Text deriving (Show, Exception, ReportableException)

instructionStringToInputs :: Text -> Either BadInstructionSet (ToggleCode, Int, Int, Int, Int)
instructionStringToInputs str = 
  let
    inputs = splitOn "," (toString str)
    parsedInputs = sequence (readEither <$> inputs)
  in 
    case parsedInputs of 
      Right [a, b, c, d, e] -> 
        let maybeAction = case a of 0 -> Right TurnOff; 1 -> Right TurnOn; 2 -> Right Toggle; _ -> Left $ BadInstructionSet $ text (printf "first instruction (%i) must be a number between 0 and 2 inclusive" a)
        in maybeAction <#> \action -> (action, b, c, d, e)
      _ -> Left $ BadInstructionSet $ text (printf "instructionSet: '%s' must be a set of 5, comma separated integers" str)

evaluateInstructionSet :: [(ToggleCode, Int, Int, Int, Int)] -> Text
evaluateInstructionSet validInputs = show $ execute initialGrid validInputs 

main :: IO ()
main = do
  
  answers <- loadAllCases
  forM_ answers putTextLn

  where
    loadAllCases :: IO [Text]
    loadAllCases = do
      -- let read = readInt >>> first (const $ throw $ BadNumberOfRuns "each run must begin with a line with a single integer for the number of cases")
      n <- readEither <$> getLine
      let nCases = n # Either.unless "each run must begin with a line with a single integer for the number of cases"
      loadAllInstructions nCases

    loadAllInstructions :: Int -> IO [Text]
    loadAllInstructions nCases =
      sequence (const loadOneSetOfInstructions <$> [1 .. nCases])

    loadOneSetOfInstructions :: IO Text
    loadOneSetOfInstructions = do
      n <- readEither <$> getLine
      let nInstructions = n # Either.unless "each case must begin with a line with a single integer for the number of instructions"
      evaluateAllInstructions nInstructions

    evaluateAllInstructions :: Int -> IO Text
    evaluateAllInstructions n = do 
      instructions <- sequence (const getLine <$> [1 .. n])
      let 
        inputs = sequence (instructionStringToInputs <$> instructions)
        i = Either.expect inputs
      pure $ evaluateInstructionSet i

