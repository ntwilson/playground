module Main

import Data.Vect
import Data.String

infixl 1 #

(#) : a -> (a -> b) -> b
x # f = f x


fromIntegral : Integer -> Double
fromIntegral = cast

floorToInt : Double -> Integer
floorToInt = cast . floor

round : Double -> Integer
round d = floorToInt (d + 0.5) 

totalFlightTime : Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
totalFlightTime distance velocity departureHr departureMin = 
  let 
    totalDurationInHours = (fromIntegral distance) / (fromIntegral velocity)
    departureTimeInHours = (fromIntegral departureHr) + (fromIntegral departureMin / 60.0)
    totalTimeInHours = departureTimeInHours + totalDurationInHours
    hours = floorToInt totalTimeInHours
    fractionalHour = totalTimeInHours - (fromIntegral hours)
    minutes = round $ fractionalHour * 60
  in (hours, minutes)
  

caseStringToInputs : String -> Either String (Integer, Integer, Integer, Integer)
caseStringToInputs str = 
  let
    inputs = words str
    parsedInputs = sequence (parsePositive <$> inputs)
  in 
    case parsedInputs of 
      Just [a, b, c, d] => Right (a, b, c, d)
      Nothing => Left $ "case string: '" ++ str ++ "' must be a set of four, space separated integers"

evaluateCase : (Integer, Integer, Integer, Integer) -> IO ()
evaluateCase (distance, velocity, departureHr, departureMin) = 
  let (arrivalHr, arrivalMin) = totalFlightTime distance velocity departureHr departureMin
  in putStrLn $ show arrivalHr ++ ":" ++ show arrivalMin

main : IO ()
main = do
  n <- (parsePositive <$> getLine)
  case n of
    Nothing => putStrLn "each run must begin with a line with a single integer for the number of cases"
    Just nCases => do
      cases <- sequence (const getLine <$> [1 .. nCases])      

      case sequence (caseStringToInputs <$> cases) of
        Left _ => putStrLn "each run must begin with a line with a single integer for the number of cases"
        Right inputs => do
          putStrLn ""
          for_ inputs evaluateCase





