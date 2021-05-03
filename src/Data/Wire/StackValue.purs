module Domination.Data.Wire.StackValue where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Domination.Data.StackEvaluation (StackValue(..))
import Domination.Data.Wire.Filter (WireFilter)
import Domination.Data.Wire.Filter as Filter
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Util ((.^))

data WireStackValue
  = WireStackArrayInt (Array WireInt)
  | WireStackInt WireInt
  | WireStackString String
  | WireStackFilter WireFilter

_toWire :: Iso' StackValue WireStackValue
_toWire = iso to from where
  to = case _ of
    StackArrayInt xs -> WireStackArrayInt (view Int._toWire <$> xs)
    StackInt x -> WireStackInt (view Int._toWire x)
    StackString s -> WireStackString s
    StackFilter filter -> WireStackFilter $ filter ^. Filter._toWire
  from = case _ of
    WireStackArrayInt xs -> StackArrayInt (review Int._toWire <$> xs)
    WireStackInt x -> StackInt (review Int._toWire x)
    WireStackString s -> StackString s
    WireStackFilter filter -> StackFilter $ filter .^ Filter._toWire

derive instance genericWireStackValue :: Generic WireStackValue _
derive instance eqWireStackValue :: Eq WireStackValue
instance showWireStackValue :: Show WireStackValue where
  show a = genericShow a
instance encodeJsonWireStackValue :: EncodeJson WireStackValue where
  encodeJson a = genericEncodeJson a
instance decodeJsonWireStackValue :: DecodeJson WireStackValue where
  decodeJson a = genericDecodeJson a
instance dynamicByteLengthWireStackValue
  :: DynamicByteLength WireStackValue where
  byteLength x = genericByteLength x
instance encodeArrayBuffeWireStackValue
  :: EncodeArrayBuffer WireStackValue where
  putArrayBuffer x = genericPutArrayBuffer x
instance decodeArrayBuffeWireStackValue
  :: DecodeArrayBuffer WireStackValue where
  readArrayBuffer x = genericReadArrayBuffer x

