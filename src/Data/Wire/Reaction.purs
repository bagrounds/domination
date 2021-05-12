module Domination.Data.Wire.Reaction where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Lens.Getter ((^.))
import Data.Lens.Iso (Iso', iso)
import Data.Show.Generic (genericShow)
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.Wire.Choice (WireChoice)
import Domination.Data.Wire.Choice as Choice
import Util ((.^))

data WireReaction
  = WireBlockAttack
  | WireReactWithChoice WireChoice

_toWire :: Iso' Reaction WireReaction
_toWire = iso to from where
  to =  case _ of
    BlockAttack -> WireBlockAttack
    ReactWithChoice choice ->
      WireReactWithChoice $ choice ^. Choice._toWire

  from = case _ of
    WireBlockAttack -> BlockAttack
    WireReactWithChoice choice ->
      ReactWithChoice $ choice .^ Choice._toWire

derive instance genericWireReaction :: Generic WireReaction _

derive instance eqWireReaction :: Eq WireReaction

instance showWireReaction :: Show WireReaction where
  show wireReaction = genericShow wireReaction

instance encodeJsonWireReaction :: EncodeJson WireReaction where
  encodeJson a = genericEncodeJson a

instance decodeJsonWireReaction :: DecodeJson WireReaction where
  decodeJson a = genericDecodeJson a

instance dynamicByteLengthWireReaction
  :: DynamicByteLength WireReaction where
  byteLength x = genericByteLength x

instance encodeArrayBuffeWireReaction
  :: EncodeArrayBuffer WireReaction where
  putArrayBuffer x = genericPutArrayBuffer x

instance decodeArrayBuffeWireReaction
  :: DecodeArrayBuffer WireReaction where
  readArrayBuffer x = genericReadArrayBuffer x

