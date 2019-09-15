module Data.List.Infinite 
  ( InfList
  , ApplicationHung(..)
  , iterate
  , take
  , takeLazy
  , find
  , index
  , mapMaybe
  , catMaybes
  , chunkBySize
  , filter
  , drop
  , dropWhile
  , head
  , uncons
  , mapWithIndex
  , zipWith
  , zipWithL, zipWithR) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Lazy (defer)
import Data.List.Lazy (List(..), Step(..), intercalate)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)

newtype InfList a = InfList (List a)

newtype ApplicationHung = ApplicationHung String

derive instance eqApplicationHung :: Eq ApplicationHung 
derive instance ordApplicationHung :: Ord ApplicationHung
derive instance newtypeApplicationHung :: Newtype ApplicationHung _
instance showApplicationHung :: Show ApplicationHung where 
  show (ApplicationHung msg) = "\"" <> msg <> "\""

hung :: ApplicationHung
hung = ApplicationHung "Program execution hung. This infinite sequence was allowed to evaluate elements for too long."

instance showInfList :: Show a => Show (InfList a) where
  show xs = 
    case take 10 xs of
      Right elementsToShow -> 
        let listSubstring = intercalate ", " (show <$> elementsToShow)
        in "InfList [" <> listSubstring <> ", ...]"
      Left hungErr -> 
        "InfList <application hung while showing the list>"
        
instance functorInfList :: Functor InfList where
  map f (InfList xs) = InfList $ map f xs

-- | create a new "infinite" list.  Lists aren't actually infinite,
-- | but bounded by a max number of elements to ensure that the program
-- | doesn't hang at runtime 
iterate :: forall a. (a -> a) -> a -> { maxElements :: Int } -> InfList a
iterate incrementor init { maxElements } = 
  InfList $ List.take maxElements $ List.iterate incrementor init

take :: forall a. Int -> InfList a -> Either ApplicationHung (List a)
take n (InfList xs) = 
  let result = List.take n xs
  in if List.length result == n then Right result else Left hung

-- | Lazily returns up to the first N elements of the sequence.  
-- | Note that reaching the end of the infinite sequence represents
-- | the application hanging, and we cannot preemptively detect a hang while executing lazily.
-- | As such the possibility of a hang is deferred to each individual element.
-- | This only returns elements up to the first Error, so there is no guarantee that
-- | the resulting sequence would contain N elements.  
-- | If you are able to eagerly evaluate the first n elements, consider using 
-- | `take` instead, which is likely easier to consume.
takeLazy :: forall a. Int -> InfList a -> List (Either ApplicationHung a)
takeLazy n (InfList xs) =
  lazySnoc (Right <$> xs) (Left hung)
  # List.take n
  
takeWhile :: forall a. (a -> Boolean) -> InfList a -> Either ApplicationHung (List a)
takeWhile predicate infxs@(InfList xs) = 
  find (not <<< predicate) infxs
  <#> (const $ List.takeWhile predicate xs)

lazySnoc :: forall a. List a -> a -> List a
lazySnoc xs x = 
  List $ defer \_ ->
    case List.uncons xs of
      Just { head: h, tail } -> Cons h (lazySnoc tail x)
      Nothing -> Cons x List.nil


takeWhileLazy :: forall a. (a -> Boolean) -> InfList a -> List (Either ApplicationHung a)
takeWhileLazy predicate (InfList xs) = 
  lazySnoc (Right <$> xs) (Left hung)
  # List.takeWhile (case _ of Left _ -> true
                              Right x -> predicate x)


index :: forall a. Int -> InfList a -> Either ApplicationHung a
index i (InfList xs) = 
  note hung $ List.index xs i

find :: forall a. (a -> Boolean) -> InfList a -> Either ApplicationHung a
find predicate (InfList xs) = note hung $ go xs
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
  listChunkBySize size xs
  <#> (\chunk -> if List.length chunk == size then Just chunk else Nothing)
  # List.takeWhile isJust
  # List.catMaybes
  # InfList

filter :: forall a. (a -> Boolean) -> InfList a -> InfList a 
filter predicate (InfList xs) = InfList $ List.filter predicate xs

drop :: forall a. Int -> InfList a -> InfList a
drop n (InfList xs) = InfList $ List.drop n xs

dropWhile :: forall a. (a -> Boolean) -> InfList a -> InfList a
dropWhile predicate (InfList xs) = InfList $ List.dropWhile predicate xs

head :: forall a. InfList a -> Either ApplicationHung a
head (InfList xs) = note hung $ List.head xs

uncons :: forall a. InfList a -> Either ApplicationHung { head :: a, tail :: InfList a }
uncons (InfList xs) = 
  List.uncons xs <#> (\x -> x { tail = InfList x.tail })
  # note hung
  
mapWithIndex :: forall a b. (Int -> a -> b) -> InfList a -> InfList b
mapWithIndex projection (InfList xs) = 
  InfList $ go xs 0
  where
    go xs' idx = 
      List $ defer \_ -> case List.step xs' of
        Cons h tail -> Cons (projection idx h) (go tail (idx+1))
        Nil -> Nil

zipWith :: forall a b c. (a -> b -> c) -> InfList a -> InfList b -> InfList c
zipWith fn (InfList xs) (InfList ys) = InfList $ List.zipWith fn xs ys

zipWithL :: forall a b c. (a -> b -> c) -> InfList a -> Array b -> Either ApplicationHung (Array c)
zipWithL fn (InfList xs) ys = 
  let 
    xsArr = List.take (Array.length ys) xs # Array.fromFoldable
    result = Array.zipWith fn xsArr ys
  in
    if Array.length ys == Array.length result 
    then Right $ Array.fromFoldable result
    else Left hung

zipWithR :: forall a b c. (a -> b -> c) -> Array b -> InfList a -> Either ApplicationHung (Array c)
zipWithR fn = flip (zipWithL fn)

-- tests stop here -- 

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


