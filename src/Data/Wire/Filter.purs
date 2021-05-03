module Domination.Data.Wire.Filter where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Getter ((^.))
import Data.Lens.Iso (Iso', iso)
import Domination.Data.CardType (CardType)
import Domination.Data.Filter (Filter(..))
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Util ((.^))

data WireFilter
  = WireHasName String
  | WireHasType CardType
  | WireCostUpTo WireInt
  | WireAny

_toWire :: Iso' Filter WireFilter
_toWire = iso to from where
  to = case _ of
    HasName name -> WireHasName name
    HasType cardType -> WireHasType cardType
    CostUpTo n -> WireCostUpTo $ n ^. Int._toWire
    Any -> WireAny
  from = case _ of
    WireHasName name -> HasName name
    WireHasType cardType -> HasType cardType
    WireCostUpTo n -> CostUpTo $ n .^ Int._toWire
    WireAny -> Any

derive instance genericWireFilter :: Generic WireFilter _
derive instance eqWireFilter :: Eq WireFilter
instance showWireFilter :: Show WireFilter where
  show condition = genericShow condition
instance encodeJsonWireFilter :: EncodeJson WireFilter where
  encodeJson a = genericEncodeJson a
instance decodeJsonWireFilter :: DecodeJson WireFilter where
  decodeJson a = genericDecodeJson a

instance dynamicByteLengthWireFilter
  :: DynamicByteLength WireFilter where
  byteLength = genericByteLength
instance encodeArrayBuffeWireFilter
  :: EncodeArrayBuffer WireFilter where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeWireFilter
  :: DecodeArrayBuffer WireFilter where
  readArrayBuffer = genericReadArrayBuffer

