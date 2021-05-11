module Domination.Data.Wire.Bonus where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens.Getter ((^.))
import Data.Lens.Iso (Iso', iso)
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Util ((.^))

data WireBonus
  = WireCash WireInt

_toWire :: Iso' Bonus WireBonus
_toWire = iso to from where
  to = case _ of
    Cash n -> WireCash $ n ^. Int._toWire
  from = case _ of
    WireCash n -> Cash $ n .^ Int._toWire

derive instance genericWireBonus :: Generic WireBonus _
derive instance eqWireBonus :: Eq WireBonus
instance showWireBonus :: Show WireBonus where show = genericShow
instance encodeJsonWireBonus :: EncodeJson WireBonus where
  encodeJson a = genericEncodeJson a
instance decodeJsonWireBonus :: DecodeJson WireBonus where
  decodeJson a = genericDecodeJson a

instance dynamicByteLengthWireBonus
  :: DynamicByteLength WireBonus where
  byteLength = genericByteLength
instance encodeArrayBuffeWireBonus :: EncodeArrayBuffer WireBonus where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeWireBonus :: DecodeArrayBuffer WireBonus where
  readArrayBuffer = genericReadArrayBuffer

