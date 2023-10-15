module Data.Stack.Primitive where

import Control.Category (class Category, identity)
import Control.Semigroupoid (class Semigroupoid)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.BraidedCategory (class BraidedCategory)
import Data.Cartesian (class Cartesian)
import Data.Cocartesian (class Cocartesian)
import Data.Eq (class Eq)
import Data.EuclideanRing (class EuclideanRing, (/))
import Data.Generic.Rep (class Generic)
import Data.MonoidalProduct (class MonoidalProduct)
import Data.Ring (class Ring, zero, (-))
import Data.Semiring (class Semiring, (*), (+))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple, uncurry)

class (Cocartesian p, Cartesian p, BraidedCategory p)
  <= StackPrimitive p where
  id :: forall a. p a a
  add :: forall a. Semiring a => p (Tuple a a) a
  mul :: forall a. Semiring a => p (Tuple a a) a
  sub :: forall a. Ring a => p (Tuple a a) a
  negate :: forall a. Ring a => p a a
  div :: forall a. EuclideanRing a => p (Tuple a a) a

newtype Primitive a b = Primitive (a -> b)

instance stackPrimitivePrimitive :: StackPrimitive Primitive where
  add = Primitive (uncurry (+))
  mul = Primitive (uncurry (*))
  sub = Primitive (uncurry (-))
  negate = Primitive (zero - _)
  div = Primitive (uncurry (/))
  id = Primitive (identity)

derive newtype instance cartesianPrimitive :: Cartesian Primitive
derive newtype instance cocartesianPrimitive :: Cocartesian Primitive
derive newtype instance braidedCategoryPrimitive
  :: BraidedCategory Primitive
derive newtype instance monoidalProductPrimitive
  :: MonoidalProduct Primitive

derive newtype instance semigroupoidPrimitive :: Semigroupoid Primitive
derive newtype instance categoryPrimitive :: Category Primitive

instance stackPrimitiveReifiedPrimitive
  :: StackPrimitive ReifiedPrimitive where
  id = Id
  add = Add
  mul = Mul
  sub = Sub
  negate = Negate
  div = Div

data ReifiedPrimitive :: forall k1 k2. k1 -> k2 -> Type
data ReifiedPrimitive a b
  = ExtractLeft
  | ExtractRight
  | Duplicate
  | InjectLeft
  | InjectRight
  | Jam
  | Swap
  | Id
  | Add
  | Mul
  | Sub
  | Negate
  | Div
  -- the "forall z." breaks the Generic typeclass instance
  -- | First (Maybe (ReifiedPrimitive a b)) (forall z. ReifiedPrimitive (Tuple a z) (Tuple b z))

instance braidedCategoryReifiedPrimitive
  :: BraidedCategory ReifiedPrimitive where
  swap = Swap

instance cartesianReifiedPrimitive :: Cartesian ReifiedPrimitive where
  extractLeft = ExtractLeft
  extractRight = ExtractRight
  duplicate = Duplicate

instance cocartesianReifiedPrimitive
  :: Cocartesian ReifiedPrimitive where
  injectLeft = InjectLeft
  injectRight = InjectRight
  jam = Jam

derive instance eqReifiedPrimitive
  :: (Eq a, Eq b)
  => Eq (ReifiedPrimitive a b)

derive instance genericReifiedPrimitive
  :: Generic (ReifiedPrimitive a b) _

instance encodeJsonReifiedPrimitive
  :: (EncodeJson a, EncodeJson b)
  => EncodeJson (ReifiedPrimitive a b) where
  encodeJson = genericEncodeJson
instance decodeJsonReifiedPrimitive
  :: (DecodeJson a, DecodeJson b)
  => DecodeJson (ReifiedPrimitive a b) where
  decodeJson = genericDecodeJson

instance dynamicByteLengthReifiedPrimitive
  :: (DynamicByteLength a, DynamicByteLength b)
  => DynamicByteLength (ReifiedPrimitive a b) where
  byteLength = genericByteLength
instance encodeArrayBuffeReifiedPrimitive
  :: (EncodeArrayBuffer a, EncodeArrayBuffer b)
  => EncodeArrayBuffer (ReifiedPrimitive a b) where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeReifiedPrimitive
  :: ( DynamicByteLength a
     , DynamicByteLength b
     , DecodeArrayBuffer a
     , DecodeArrayBuffer b
     )
  => DecodeArrayBuffer (ReifiedPrimitive a b) where
  readArrayBuffer = genericReadArrayBuffer

instance showReifiedPrimitive
  :: (Show a, Show b)
  => Show (ReifiedPrimitive a b) where
  show = genericShow

