module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (range, filter, List)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Generic (decode)
import Foreign.Object (Object, lookup)

type AzureResponse = { body :: String }
type AzureRequest = 
  { query :: Object String 
  , body :: Foreign
  }

ns :: Int -> List Int
ns max = range 0 max

multiples :: Int -> List Int
multiples max = filter (\n -> mod n 3 == 0 || mod n 5 == 0) (ns max)

answer :: Int -> Int
answer max = sum $ multiples max

main :: AzureRequest -> Effect AzureResponse
main req = do
  let
    question = 
      case lookup "i" req.query of
        Just strI -> 
          case Int.fromString strI of
            Just i -> i
            _ -> 999
        Nothing -> 
          case unwrap $ runExceptT $ decode req.body of
            Right ({i} :: {i::Int}) -> i
            Left _ -> 999

  pure { body: "Answer is " <> show (answer question) }
