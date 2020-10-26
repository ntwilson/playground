module TryStuff where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Unsafe.Coerce (unsafeCoerce)

data SomeShape

class Shape a where
  perimeter :: a -> Number
  area :: a -> Number
  asShape :: a -> SomeShape

newtype ShapeBox = ShapeBox (forall out. (forall s. Shape s => s -> out) -> out)

boxShape :: forall a. Shape a => a -> ShapeBox
boxShape x = ShapeBox \k -> k x

boxedArea :: ShapeBox -> Number
boxedArea (ShapeBox x) = x area

boxedPerimeter :: ShapeBox -> Number
boxedPerimeter (ShapeBox x) = x perimeter

boxedAsShape :: ShapeBox -> SomeShape
boxedAsShape (ShapeBox x) = x asShape

data Rectangle = Rectangle Number Number
data Square = Square Number

derive instance genericRect :: Generic Rectangle _
instance showRect :: Show Rectangle where show = genericShow
instance rectShape :: Shape Rectangle where
  perimeter (Rectangle x y) = 2.0*x + 2.0*y
  area (Rectangle x y) = x*y
  asShape = unsafeCoerce

derive instance genericSq :: Generic Square _
instance showSq :: Show Square where show = genericShow
instance sqShape :: Shape Square where
  perimeter (Square x) = 4.0*x
  area (Square x) = x*x
  asShape = unsafeCoerce

rect :: Rectangle 
rect = Rectangle 2.0 4.0

sq :: Square
sq = Square 3.0

shapes :: Array ShapeBox
shapes = [boxShape rect, boxShape sq]

areas :: Array Number
areas = boxedArea <$> shapes

perimeters :: Array Number
perimeters = boxedPerimeter <$> shapes

