{-# LANGUAGE DuplicateRecordFields, OverloadedLabels, FlexibleContexts, DataKinds, TypeApplications, FlexibleInstances, MultiParamTypeClasses #-}
module Lib
    ( someFunc
    ) where


import GHC.Records (HasField(..)) 
import GHC.OverloadedLabels (IsLabel(..))

data Person = Person { name :: String, age :: Int }
data House = House { address :: String, age :: Int }

-- these instances needed for the #age approach, but not the getField approach
instance IsLabel "age" (House -> Int) where
    fromLabel = age

instance IsLabel "age" (Person -> Int) where
    fromLabel = age

getAge r = #age r

getAge2 r = getField @"age" r

nathan = Person { name = "nathan", age = 33 }
home = House { address = "don't want to type it out", age = 105 }

someFunc :: IO ()
someFunc = do
    putStrLn $ show (getAge nathan :: Int)
    putStrLn $ show (getAge2 home :: Int)
