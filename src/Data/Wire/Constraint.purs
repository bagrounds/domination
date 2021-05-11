module Domination.Data.Wire.Constraint where

import Prim hiding (Constraint)

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens.Getter ((^.))
import Data.Lens.Iso (Iso', iso)
import Data.Show (class Show)
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Util ((.^))

data WireConstraint
  = WireUpTo WireInt
  | WireExactly WireInt
  | WireDownTo WireInt
  | WireUnlimited

_toWire :: Iso' Constraint WireConstraint
_toWire = iso to from where
  to = case _ of
    UpTo n -> WireUpTo $ n ^. Int._toWire
    Exactly n -> WireExactly $ n ^. Int._toWire
    DownTo n -> WireDownTo $ n ^. Int._toWire
    Unlimited -> WireUnlimited
  from = case _ of
    WireUpTo n -> UpTo $ n .^ Int._toWire
    WireExactly n -> Exactly $ n .^ Int._toWire
    WireDownTo n -> DownTo $ n .^ Int._toWire
    WireUnlimited -> Unlimited

derive instance genericWireConstraint :: Generic WireConstraint _
derive instance eqWireConstraint :: Eq WireConstraint

instance showWireConstraint :: Show WireConstraint where
  show = genericShow
instance encodeJsonWireConstraint :: EncodeJson WireConstraint where
  encodeJson = genericEncodeJson
instance decodeJsonWireConstraint :: DecodeJson WireConstraint where
  decodeJson = genericDecodeJson

instance dynamicByteLengthWireConstraint
  :: DynamicByteLength WireConstraint where
  byteLength = genericByteLength
instance encodeArrayBuffeWireConstraint
  :: EncodeArrayBuffer WireConstraint where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeWireConstraint
  :: DecodeArrayBuffer WireConstraint where
  readArrayBuffer = genericReadArrayBuffer

