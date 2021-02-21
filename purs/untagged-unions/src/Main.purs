module Main where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (type (|+|), toEither1)

infixr 7 type Either as ||

data ChoiceOf3 a b c
  = OneOf3 a
  | TwoOf3 b
  | ThreeOf3 c

derive instance eqChoice :: (Eq a, Eq b, Eq c) => Eq (ChoiceOf3 a b c)
derive instance ordChoice :: (Ord a, Ord b, Ord c) => Ord (ChoiceOf3 a b c)
derive instance genericChoice :: Generic (ChoiceOf3 a b c) _
instance showChoice :: (Show a, Show b, Show c) => Show (ChoiceOf3 a b c) where show = genericShow

tag :: forall a b c. HasRuntimeType a => HasRuntimeType b => HasRuntimeType c =>
  (a |+| b |+| c) -> ChoiceOf3 a b c
tag x = case tag' x of
  Left anA -> OneOf3 anA
  Right (Left aB) -> TwoOf3 aB
  Right (Right aC) -> ThreeOf3 aC

tag' :: forall a b c. HasRuntimeType a => HasRuntimeType b => HasRuntimeType c =>
  (a |+| b |+| c) -> (a || b || c) 
tag' x = toEither1 <$> toEither1 x

main :: Effect Unit
main = do
  log "🍝"



infixr 5 type Map as |->

bindLookup :: forall key value. Ord key => Maybe (key |-> value) -> key -> Maybe value
bindLookup xs key = lookup key =<< xs

infixl 5 bindLookup as <*|

fn :: (Int |-> String |-> Boolean) -> Int -> String -> Maybe Boolean
fn xs date fp = Just xs <*| date <*| fp

