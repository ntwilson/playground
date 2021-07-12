module ObjectClass where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (Variant, case_, expand, inj, on, prj)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (RowList)
import Prim.RowList as RL
import Record as Record
import Type.Prelude (class IsSymbol, Proxy(..))

type NoSubtype :: Row Type -> Row Type
type NoSubtype r = (noSubtype :: Unit | r)
noSubtype :: ∀ r. Variant (NoSubtype r)
noSubtype = inj (Proxy :: _ "noSubtype") unit

type ObjectClass :: Row Type -> Row Type -> Type
type ObjectClass shared subtypes = 
  Record ( subtype :: Variant subtypes | shared )

instanceOf :: ∀ sym shared subtypes subtype combinedWithDupes combined tail. 
  IsSymbol sym => Union shared subtype combinedWithDupes => Nub combinedWithDupes combined => 
  Lacks "subtype" shared => Cons sym (Record subtype) tail subtypes =>
  Proxy sym -> ObjectClass shared subtypes -> Maybe (Record combined)
instanceOf proxy cls = do
  subtype <- prj proxy cls.subtype
  pure $ Record.merge (Record.delete (Proxy :: _ "subtype") cls) subtype
  
cast :: forall sym subtypes shared subtype combinedWithDupes combined.
  Cons sym (Record subtype) () subtypes => IsSymbol sym => 
  Union shared subtype combinedWithDupes => Nub combinedWithDupes combined => Lacks "subtype" shared => 
  Proxy sym -> ObjectClass shared subtypes -> Record combined
cast proxy cls = Record.merge (Record.delete (Proxy :: _ "subtype") cls) subtypeVals
  where
    subtypeVals = (case_ # on proxy identity) cls.subtype

newLeaf :: ∀ shared r. Lacks "subtype" shared => Record shared -> ObjectClass shared (NoSubtype r)
newLeaf rec = Record.insert (Proxy :: _ "subtype") noSubtype rec


new :: ∀ sym shared subtypes combined tail subtype sharedList subtypeList. 
  IsSymbol sym => Lacks "subtype" shared => Cons sym (Record subtype) tail subtypes => 
  ExtractFields sharedList combined shared => ExtractFields subtypeList combined subtype => 
  Union shared subtype combined => 
  RL.RowToList shared sharedList => RL.RowToList subtype subtypeList => 
  Proxy sym -> Record combined -> ObjectClass shared subtypes 
new proxy rec = 
  extractFields (Proxy :: _ sharedList) rec 
  # Record.insert (Proxy :: _ "subtype") (inj proxy $ extractFields (Proxy :: _ subtypeList) rec)

class ExtractFields :: RowList Type -> Row Type -> Row Type -> Constraint
class ExtractFields list combined out | list -> out where
  extractFields :: Proxy list -> Record combined -> Record out

instance ExtractFields RL.Nil combined () where
  extractFields _ _ = { } 

else instance 
  ( Cons label value rowCombinedTail combined
  , Cons label value rowOutTail out
  , IsSymbol label
  , RL.RowToList rowCombinedTail combinedTail
  , RL.RowToList rowOutTail outTail
  , ExtractFields outTail combined rowOutTail
  , Lacks label rowOutTail
  ) => 
  ExtractFields (RL.Cons label value outTail) combined out where 
  extractFields :: Proxy (RL.Cons label value outTail) -> Record combined -> Record out
  extractFields _ rec = Record.insert label value $ extractFields (Proxy :: Proxy outTail) rec
    where
      label = (Proxy :: _ label)
      value = Record.get label rec


  
expandSubtype :: ∀ shared lt a gt. Union lt a gt => ObjectClass shared lt -> ObjectClass shared gt
expandSubtype cl@{subtype} = cl { subtype = expand subtype }

type PointData r = ( x :: Int, y :: String | r )
type Point subtype = ObjectClass (PointData ()) subtype

a :: Point (NoSubtype ()) 
a = newLeaf { x: 5, y: "yo" }

type ThreeDPointData r = { z :: Number | r }
type ThreeDPoint r = Point ( "3DPoint" :: ThreeDPointData () | r )
type Direct3DPoint = ThreeDPointData (PointData ())
as3DPoint :: ∀ subtype. ThreeDPoint subtype -> Maybe Direct3DPoint
as3DPoint = instanceOf (Proxy :: _ "3DPoint")

threeDPoint :: ∀ r. Direct3DPoint -> ThreeDPoint r
threeDPoint = new (Proxy :: _ "3DPoint")

b :: ThreeDPoint ()
b = threeDPoint { x: 5, y: "yo", z: 8.2 }

points :: Array (Point _) 
points = [ expandSubtype a, expandSubtype b ]
