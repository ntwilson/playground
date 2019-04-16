module Lib (totalFlightTime) where

totalFlightTime :: Int -> Int -> Int -> Int -> (Int, Int)
totalFlightTime distance velocity departureHr departureMin = 
  let 
    totalDurationInHours = (fromIntegral distance) / (fromIntegral velocity)
    departureTimeInHours = (fromIntegral departureHr) + (fromIntegral departureMin / 60.0)
    totalTimeInHours = departureTimeInHours + totalDurationInHours
    hours = floor totalTimeInHours
    fractionalHour = totalTimeInHours - (fromIntegral hours)
    minutes = round $ fractionalHour * 60
  in (hours, minutes)
