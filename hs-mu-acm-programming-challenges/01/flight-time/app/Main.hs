module Main where

import Lib
import Data.Text.Read
import Text.Printf (printf)
import qualified Either
import qualified Maybe

text :: String -> Text
text = toText

caseStringToInputs :: Text -> Either Text (Int, Int, Int, Int)
caseStringToInputs str = 
  let
    inputs = words str
    parsedInputs = sequence (readEither <$> inputs)
  in 
    case parsedInputs of 
      Right [a, b, c, d] -> Right (a, b, c, d)
      Left _ -> Left $ text (printf "case string: '%s' must be a set of four, space separated integers" (toString str))

main :: IO ()
main = do
  n <- (readEither <$> getLine)
  nCases <- n # Either.unless (text "each run must begin with a line with a single integer for the number of cases")

  cases <- sequence (const getLine <$> [1 .. nCases])
  inputs <- sequence (caseStringToInputs <$> cases) # Either.expect

  putTextLn $ toText ""
  forM_ inputs $ \(distance, velocity, departureHr, departureMin) ->
    let (arrivalHr, arrivalMin) = totalFlightTime distance velocity departureHr departureMin
    in printf "%i:%02i\n" arrivalHr arrivalMin
  
