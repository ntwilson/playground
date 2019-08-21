module Main 
  ( main
  , encrypt
  , chunkBySize) where

import Prelude

import Data.Array as Array
import Data.Int (ceil)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Promise (class Deferred, Promise, promise, runPromise)
import Math (sqrt)
import Node.ReadLine (close, createConsoleInterface, noCompletion, question)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

chunkBySize :: forall a. Int -> List a -> List (List a)
chunkBySize size Nil = Nil
chunkBySize size xs 
  | List.length xs <= size = List.singleton xs 
  | otherwise = List.take size xs : (chunkBySize size $ List.drop size xs)

encrypt :: String -> String 
encrypt s = 
  let 
    chars = toCharArray s
    noWhitespace = chars # Array.filter (not <<< isWhitespace) # List.fromFoldable 
    nCols = ceil $ sqrt $ Int.toNumber $ List.length noWhitespace
    grid = chunkBySize nCols noWhitespace 
    flipped = List.transpose grid
    
  in List.intercalate (List.singleton ' ') flipped # Array.fromFoldable # fromCharArray

  where
    isWhitespace ' '  = true
    isWhitespace '\n' = true
    isWhitespace '\t' = true
    isWhitespace _    = false


ask :: Deferred => String -> Promise String
ask prompt = 
  promise \resolve err -> do
    iface <- createConsoleInterface noCompletion
    let 
      resolveAndClose x = do
        resolve x
        close iface

    question prompt resolveAndClose iface

fromJustUnless :: forall a. Partial => String -> Maybe a -> a
fromJustUnless msg (Just x) = x
fromJustUnless msg Nothing = crashWith msg

getInput :: Partial => Deferred => Promise (List String)
getInput = do
  testNumStr <- ask "how many phrases would you like to encrypt? "
  let testNum = fromJustUnless "Unrecognized input.  Expecting a number." $ Int.fromString testNumStr

  -- I'd much rather just write
  -- for (1 .. testNum) \i -> ask ("test case #" <> show i <> " ")
  -- but it tries to run all promises concurrently, and I need to sequence them
  let 
    iterate cases i 
      | i == testNum = do
        thisCase <- ask ("test case #" <> show i <> " ")
        pure $ List.snoc cases thisCase
      | otherwise = do
        thisCase <- ask ("test case #" <> show i <> " ") 
        let newCases = List.snoc cases thisCase
        iterate newCases (i + 1)

  iterate Nil 1

main :: Effect Unit
main = unsafePartial $
  runPromise
    (\testCases -> do
      log ""
      for_ testCases $ log <<< encrypt)
    (\err -> logShow err)
    getInput
