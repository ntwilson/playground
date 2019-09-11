module Data.List.Infinite 
  ( InfList
  , iterate
  , take
  , find
  , index
  , mapMaybe
  , catMaybes
  , chunkBySize) where

import Prelude

import Data.Lazy (defer)
import Data.List.Lazy (List(..), Step(..), intercalate)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))

newtype InfList a = InfList (List a)

instance showInfList :: Show a => Show (InfList a) where
  show (InfList xs) = "InfList [" <> listSubstring <> ", ...]"
    where 
      listSubstring = 
        let elementsToShow = List.take 10 xs
        in intercalate ", " (show <$> elementsToShow)
        
instance functorInfList :: Functor InfList where
  map f (InfList xs) = InfList $ map f xs

-- | create a new "infinite" list.  Lists aren't actually infinite,
-- | but bounded by a max number of elements to ensure that the program
-- | doesn't hang at runtime 
iterate :: forall a. (a -> a) -> a -> { maxElements :: Int } -> InfList a
iterate incrementor init { maxElements } = 
  InfList $ List.take maxElements $ List.iterate incrementor init

take :: forall a. Int -> InfList a -> List a
take n (InfList xs) = List.take n xs

takeWhile :: forall a. (a -> Boolean) -> InfList a -> List a 
takeWhile predicate (InfList xs) = 
  List.takeWhile predicate xs

index :: forall a. Int -> InfList a -> Maybe a
index i (InfList xs) = List.index xs i

find :: forall a. (a -> Boolean) -> InfList a -> Maybe a
find predicate (InfList xs) = go xs
  where
    go xs' = case List.uncons xs' of
      Just { head: h, tail } | predicate h -> Just h
      Just { head: h, tail } | otherwise -> go tail
      Nothing -> Nothing


mapMaybe :: forall a b. (a -> Maybe b) -> InfList a -> InfList b
mapMaybe projection (InfList xs) =
  InfList $ List.mapMaybe projection xs 

catMaybes :: forall a. InfList (Maybe a) -> InfList a
catMaybes (InfList xs) = InfList $ List.catMaybes xs

listChunkBySize :: forall a. Int -> List a -> List (List a)
listChunkBySize size xs =
  List $ defer \_ -> Cons (List.take size xs) (listChunkBySize size $ List.drop size xs)

chunkBySize :: forall a. Int -> InfList a -> InfList (List a)
chunkBySize size (InfList xs) = 
  InfList $ listChunkBySize size xs

drop :: forall a. Int -> InfList a -> InfList a
drop n (InfList xs) = InfList $ List.drop n xs

dropWhile :: forall a. (a -> Boolean) -> InfList a -> InfList a
dropWhile predicate (InfList xs) = InfList $ List.dropWhile predicate xs

head :: forall a. InfList a -> Maybe a
head (InfList xs) = List.head xs

uncons :: forall a. InfList a -> Maybe { head :: a, tail :: InfList a }
uncons (InfList xs) = 
  List.uncons xs <#> \x -> x { tail = InfList x.tail }

mapWithIndex :: forall a b. (Int -> a -> b) -> InfList a -> InfList b
mapWithIndex projection (InfList xs) = 
  InfList $ go xs 0
  where
    go xs' idx = 
      List $ defer \_ -> case List.step xs' of
        Cons h tail -> Cons (projection idx h) (go tail (idx+1))
        Nil -> Nil

filter :: forall a. (a -> Boolean) -> InfList a -> InfList a 
filter predicate (InfList xs) = InfList $ List.filter predicate xs

pairwise :: forall a. InfList a -> InfList { left :: a, right :: a }
pairwise (InfList xs) = 
  InfList $ go xs
  where
    go xs' = 
      List $ defer \_ -> case List.step xs' of
        Cons left ys -> case List.step ys of
          Cons right zs -> Cons { left, right } (go ys)
          Nil -> Nil
        Nil -> Nil


