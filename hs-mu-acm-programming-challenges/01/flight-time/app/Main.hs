module Main where

import Lib
import Data.Text.Read
import Text.Printf (printf)


caseStringToInputs :: Text -> Maybe (Int, Int, Int, Int)
caseStringToInputs str = 
  let
    inputs = words str
    parsedInputs = sequence (readEither <$> inputs)
  in 
    case parsedInputs of 
      Right [a, b, c, d] -> Just (a, b, c, d)
      _ -> Nothing

main :: IO ()
main = do
  nCases <- (readEither <$> getLine)
  case nCases of
    Right n -> do
      cases <- sequence (const getLine <$> [1 .. n])
      let inputs = sequence (caseStringToInputs <$> cases)
      case inputs of 
        Just validInputs -> do
          putTextLn $ toText ""
          forM_ validInputs $ \(distance, velocity, departureHr, departureMin) ->
            let (arrivalHr, arrivalMin) = totalFlightTime distance velocity departureHr departureMin
            in printf "%i:%02i\n" arrivalHr arrivalMin

        Nothing -> putTextLn $ toText "bad input"

    Left _ -> putTextLn $ toText "bad input"
  
