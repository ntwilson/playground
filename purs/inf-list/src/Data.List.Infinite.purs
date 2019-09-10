module Data.List.Infinite 
  ( InfList
  , iterate
  , take
  , find
  , index) where

import Prelude

import Data.List.Lazy (List, intercalate)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))

newtype InfList a = InfList (List a)

instance showInfList :: Show a => Show (InfList a) where
  show (InfList xs) = "InfList [" <> listSubstring <> ", ...]"
    where 
      listSubstring = 
        let elementsToShow = List.take 10 xs
        in intercalate ", " (show <$> elementsToShow)

-- | create a new "infinite" list.  Lists aren't actually infinite,
-- | but bounded by a max number of elements to ensure that the program
-- | doesn't hang at runtime 
iterate :: forall a. (a -> a) -> a -> { maxElements :: Int } -> InfList a
iterate incrementor init { maxElements } = 
  InfList $ List.take maxElements $ List.iterate incrementor init

take :: forall a. Int -> InfList a -> List a
take n (InfList xs) = List.take n xs

index :: forall a. Int -> InfList a -> Maybe a
index i (InfList xs) = List.index xs i

find :: forall a. (a -> Boolean) -> InfList a -> Maybe a
find predicate (InfList xs) = go xs
  where
    go xs' = case List.uncons xs' of
      Just { head, tail } | predicate head -> Just head
      Just { head, tail } | otherwise -> go tail
      Nothing -> Nothing

