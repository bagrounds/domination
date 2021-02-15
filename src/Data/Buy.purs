module Domination.Data.Buy where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((^.))
import Data.Lens.Iso (Iso', iso)
import Domination.Data.WireInt (WireInt, _WireInt)
import Util ((.^))

newtype Buy = Buy WireInt

derive newtype instance eqBuy :: Eq Buy
derive instance genericBuy :: Generic Buy _
instance showBuy :: Show Buy where
  show = genericShow
instance encodeJsonBuy :: EncodeJson Buy where
  encodeJson = genericEncodeJson
instance decodeJsonBuy :: DecodeJson Buy where
  decodeJson = genericDecodeJson
derive newtype instance encodeArrayBufferBuy
  :: EncodeArrayBuffer Buy
derive newtype instance decodeArrayBufferBuy
  :: DecodeArrayBuffer Buy
derive newtype instance dynamicByteLengthBuy
  :: DynamicByteLength Buy

_wireInt :: Iso' Buy WireInt
_wireInt = iso (\(Buy w) -> w) Buy

_int :: Iso' Buy Int
_int = iso to from where
  to (Buy wireInt) = wireInt .^ _WireInt
  from = (_ ^. _WireInt) >>> (_ .^ _wireInt)

