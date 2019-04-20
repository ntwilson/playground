module Lib (execute, ToggleCode(..), LightGrid) where

import Data.List.Index

type LightGrid = [[Bool]]

data ToggleCode 
  = TurnOn
  | TurnOff
  | Toggle

execute :: LightGrid -> [(ToggleCode, Int, Int, Int, Int)] -> Int
execute initialGrid instructions =  
  let 
    finalGrid = 
      foldl' 
        (\currentGrid (a, b, c, d, e) -> mapLightGrid currentGrid a b c d e)
        initialGrid
        instructions

    numBulbsOnForRow row = 
      length $ filter id row

  in sum [ numBulbsOnForRow row | row <- finalGrid ]


mapLightGrid :: LightGrid -> ToggleCode -> Int -> Int -> Int -> Int -> LightGrid
mapLightGrid previousGrid code startX startY width height = 
  let 
    act bulb = 
      case code of 
        TurnOff -> False
        TurnOn -> True
        Toggle -> not bulb

    between x (a, b) = a <= x && x <= b

    isInActionRange xIndex yIndex = 
      xIndex `between` (startX, startX + width - 1)
      &&
      yIndex `between` (startY, startY + height - 1)

    mapBulb bulb xIndex yIndex = 
      if isInActionRange xIndex yIndex then act bulb else bulb

    mapRow (yIndex, rowVals) =
      [ mapBulb bulb xIndex yIndex | (xIndex, bulb) <- indexed rowVals]

  in 
    [ mapRow (yIndex, rowVals) | (yIndex, rowVals) <- indexed previousGrid ]
    
