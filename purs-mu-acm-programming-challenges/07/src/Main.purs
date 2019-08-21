module Main 
  ( main
  , encrypt
  , chunkBySize) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (ceil)
import Data.Int as Int
import Data.List (List(..), (:), (..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Math (sqrt)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion, question)
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


ask :: Interface -> String -> Aff String
ask iface prompt = 
  makeAff \callback -> do
    -- iface <- createConsoleInterface noCompletion
    let 
      resolveAndClose userResponse = do
        callback (Right userResponse)
        -- close iface

    question prompt resolveAndClose iface
    mempty


fromJustUnless :: forall a. Partial => String -> Maybe a -> a
fromJustUnless msg (Just x) = x
fromJustUnless msg Nothing = crashWith msg

getInput :: Partial => Aff (List String)
getInput = do
  iface <- liftEffect $ createConsoleInterface noCompletion 
  let ask' = ask iface
  testNumStr <- ask' "how many phrases would you like to encrypt? "
  let testNum = fromJustUnless "Unrecognized input.  Expecting a number." $ Int.fromString testNumStr

  ans <- for (1 .. testNum) \i -> ask' ("test case #" <> show i <> " ")
  liftEffect $ close iface
  pure ans 

run :: Partial => Aff Unit 
run = do
  testCases <- getInput
  liftEffect (log "")
  liftEffect $ for_ testCases $ log <<< encrypt

main :: Effect Unit
main = unsafePartial $
  launchAff_ run
