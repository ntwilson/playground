module Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.List (List, (..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion, question)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)


ask :: Interface -> String -> Aff String
ask iface prompt = 
  makeAff \callback -> do
    let 
      resolveAndClose userResponse = do
        callback (Right userResponse)

    question prompt resolveAndClose iface
    mempty


fromJustUnless :: forall a. Partial => String -> Maybe a -> a
fromJustUnless msg (Just x) = x
fromJustUnless msg Nothing = crashWith msg

getInput :: Partial => Aff (List (List String))
getInput = do
  iface <- liftEffect $ createConsoleInterface noCompletion 
  let ask' = ask iface
  testNumStr <- ask' "how many problems would you like to check errors for? "
  let testNum = fromJustUnless "Unrecognized input.  Expecting a number." $ Int.fromString testNumStr

  ans <- for (1 .. testNum) \i -> do
    lineNumStr <- ask' "how many lines are in this problem? "
    let lineNum = fromJustUnless "Unrecognized input.  Expecting a number." $ Int.fromString lineNumStr
    for (1 .. lineNum) \_ -> ask' ""

  liftEffect $ close iface
  pure ans 

run :: Partial => Aff Unit 
run = do
  testCases <- getInput
  liftEffect (log "")
  liftEffect $ for_ testCases $ logShow

main :: Effect Unit
main = unsafePartial $
  launchAff_ run
