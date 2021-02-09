module Domination.Data.Constraint where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Lens (Lens', lens')
import Data.Tuple (Tuple(..))
import Domination.Data.WireInt (WireInt)

data Constraint
  = UpTo WireInt
  | Exactly WireInt
  | DownTo WireInt

derive instance genericConstraint :: Generic Constraint _
derive instance eqConstraint :: Eq Constraint

instance showConstraint :: Show Constraint where
  show = genericShow
instance encodeJsonConstraint :: EncodeJson Constraint where
  encodeJson = genericEncodeJson
instance decodeJsonConstraint :: DecodeJson Constraint where
  decodeJson = genericDecodeJson

instance dynamicByteLengthConstraint
  :: DynamicByteLength Constraint where
  byteLength = genericByteLength
instance encodeArrayBuffeConstraint
  :: EncodeArrayBuffer Constraint where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeConstraint
  :: DecodeArrayBuffer Constraint where
  readArrayBuffer = genericReadArrayBuffer

_n :: Lens' Constraint WireInt
_n = lens' case _ of
  UpTo n -> Tuple n UpTo
  Exactly n -> Tuple n Exactly
  DownTo n -> Tuple n DownTo

