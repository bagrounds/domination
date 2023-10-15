module Data.Stack.Operation where

import Control.Category (class Category, (<<<))
import Control.Semigroupoid (class Semigroupoid)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.AssociativeCategory (lassoc, rassoc)
import Data.Eq (class Eq)
import Data.EuclideanRing (class EuclideanRing, (/))
import Data.Function (flip, ($))
import Data.Function as Function
import Data.Generic.Rep (class Generic)
import Data.MonoidalProduct (first)
import Data.Ring (class Ring, (-))
import Data.Semiring (class Semiring, (*), (+))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.Stack.Primitive (class StackPrimitive, Primitive(..), ReifiedPrimitive(..))
import Data.Stack.Primitive as Primitive
import Data.Tuple (Tuple(..), uncurry)

class StackPrimitive p <= StackOperation op p a b | op -> p where
  primitive :: p a b -> forall z. op (Tuple a z) (Tuple b z)
  push :: forall z. op (Tuple (Tuple a b) z) (Tuple a (Tuple b z))
  pop :: forall z. op (Tuple a (Tuple b z)) (Tuple (Tuple a b) z)

newtype Operation a b = Operation (a -> b)

derive newtype instance semigroupoidOperation :: Semigroupoid Operation
derive newtype instance categoryOperation :: Category Operation

instance stackOperationOperation
  :: StackOperation Operation Primitive a b where
  primitive (Primitive f) = Operation (first f)
  push = Operation rassoc
  pop = Operation lassoc

data ReifiedOperation :: forall k1 k2. k1 -> k2 -> Type
data ReifiedOperation a b
  = ReifiedPrimitiveOp (ReifiedPrimitive a b)
  | ReifiedPush
  | ReifiedPop

instance stackOperationReifiedOperation
  :: StackOperation ReifiedOperation ReifiedPrimitive a b where
  primitive ExtractLeft = ReifiedPrimitiveOp ExtractLeft
  primitive ExtractRight = ReifiedPrimitiveOp ExtractRight
  primitive Duplicate = ReifiedPrimitiveOp Duplicate
  primitive InjectLeft = ReifiedPrimitiveOp InjectLeft
  primitive InjectRight = ReifiedPrimitiveOp InjectRight
  primitive Jam = ReifiedPrimitiveOp Jam
  primitive Swap = ReifiedPrimitiveOp Swap
  primitive Id = ReifiedPrimitiveOp Id
  primitive Add = ReifiedPrimitiveOp Add
  primitive Mul = ReifiedPrimitiveOp Mul
  primitive Sub = ReifiedPrimitiveOp Sub
  primitive Negate = ReifiedPrimitiveOp Negate
  primitive Div = ReifiedPrimitiveOp Div
  push = ReifiedPush
  pop = ReifiedPop

derive instance eqReifiedOperation
  :: (Eq a, Eq b)
  => Eq (ReifiedOperation a b)

derive instance genericReifiedOperation
  :: Generic (ReifiedOperation a b) _

instance encodeJsonReifiedOperation
  :: (EncodeJson a, EncodeJson b)
  => EncodeJson (ReifiedOperation a b) where
  encodeJson = genericEncodeJson
instance decodeJsonReifiedOperation
  :: (DecodeJson a, DecodeJson b)
  => DecodeJson (ReifiedOperation a b) where
  decodeJson = genericDecodeJson

instance dynamicByteLengthReifiedOperation
  :: (DynamicByteLength a, DynamicByteLength b)
  => DynamicByteLength (ReifiedOperation a b) where
  byteLength = genericByteLength
instance encodeArrayBuffeReifiedOperation
  :: (EncodeArrayBuffer a, EncodeArrayBuffer b)
  => EncodeArrayBuffer (ReifiedOperation a b) where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeReifiedOperation
  :: ( DecodeArrayBuffer a
     , DecodeArrayBuffer b
     , DynamicByteLength a
     , DynamicByteLength b
     )
  => DecodeArrayBuffer (ReifiedOperation a b) where
  readArrayBuffer = genericReadArrayBuffer

instance showReifiedOperation
  :: (Show a, Show b)
  => Show (ReifiedOperation a b) where
  show = genericShow

type PrimOp a b = forall z. Operation (Tuple a z) (Tuple b z)

primOp :: forall a b. (a -> b) -> PrimOp a b
primOp f = primitive (Primitive f)

type PrimBinOp a b c = PrimOp (Tuple a b) c

primBinOp :: forall a b c. (a -> b -> c) -> PrimBinOp a b c
primBinOp f = primitive $ Primitive (uncurry f)

tuple :: forall a b. b -> PrimOp a (Tuple b a)
tuple = primOp <<< Tuple

mul :: forall n. Semiring n => PrimBinOp n n n
mul = primitive Primitive.mul

mulN :: forall n. Semiring n => n -> PrimOp n n
mulN = primOp <<< (*)

add :: forall n. Semiring n => PrimBinOp n n n
add = primitive Primitive.add

addN :: forall n. Semiring n => n -> PrimOp n n
addN = primOp <<< (+)

sub :: forall n. Ring n => PrimBinOp n n n
sub = primitive Primitive.sub

subN :: forall n. Ring n => n -> PrimOp n n
subN = primOp <<< flip (-)

div :: forall n. EuclideanRing n => PrimBinOp n n n
div = primitive Primitive.div

divN :: forall n. EuclideanRing n => n -> PrimOp n n
divN = primOp <<< flip (/)

const :: forall a b z. b -> Operation (Tuple a z) (Tuple b z)
const = primitive <<< const' where
  const' :: b -> Primitive a b
  const' = Primitive <<< Function.const

id :: forall a. PrimOp a a
id = primitive Primitive.id

