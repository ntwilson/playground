{-# LANGUAGE ScopedTypeVariables #-}
module Lib (pairwise, splitPairwise, lookAndSay, lookAndSay3x, lookAndSayAndSum) where

pairwise :: [a] -> [(a,a)]
pairwise xs@(_hd : tl) = zip xs $ tl
pairwise [] = []

-- splitPairwise (==) [1,1,2,2,2,3,4,5,6,6] = [[1], [1,2], [2], [2,3,4,5,6], [6]]
splitPairwise :: forall a. (a -> a -> Bool) -> [a] -> [NonEmpty a]
splitPairwise _splitBetween [] = []
splitPairwise splitBetween (head' : tail') = 
  let
    singleton :: a -> NonEmpty a 
    singleton x = x :| []
    
    split :: [a] -> a -> NonEmpty a -> [NonEmpty a] -> [NonEmpty a]
    split input previousElement currentGroup completedGroups =
      case input of
        [] -> completedGroups ++ [currentGroup]
        head'' : tail'' ->
          let
            newInput = tail''
            newPrev = head''
          in
            if splitBetween previousElement head''
            then 
              let 
                newGroup = singleton head''
                newCompletedGroups = completedGroups ++ [currentGroup]
              in split newInput newPrev newGroup newCompletedGroups
            else 
              let expandedGroup = currentGroup <> singleton head''
              in split newInput newPrev expandedGroup completedGroups

              
  in 
    split tail' head' (singleton head') []

unsafeToInt :: Char -> Int 
unsafeToInt char = 
  case readEither [char] of 
    Right x -> x
    Left _ -> error (toText $ "'" ++ [char] ++ "' is not a number")

lookAndSay :: String -> String
lookAndSay inputStr = 
  let
    groups = splitPairwise (/=) inputStr
    ans = do
      grp <- groups
      (show $ length grp) ++ [head grp]

  in ans
  
lookAndSay3x :: String -> String
lookAndSay3x i = lookAndSay $ lookAndSay $ lookAndSay i 
  
lookAndSayAndSum :: String -> String
lookAndSayAndSum i = 
  let
    digits = unsafeToInt <$> lookAndSay3x i
  in 
    show $ sum (digits :: [Int])