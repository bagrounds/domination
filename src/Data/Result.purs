module Domination.Data.Result where

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
import Domination.Data.WireInt (WireInt, _WireInt)
import Util ((.^))

data Result
  = Victory Int
  | Tie (Array Int)

derive instance genericResult :: Generic Result _
derive instance eqResult :: Eq Result
instance showResult :: Show Result where
  show result = genericShow result
instance encodeJsonResult :: EncodeJson Result where
  encodeJson a = genericEncodeJson a
instance decodeJsonResult :: DecodeJson Result where
  decodeJson a = genericDecodeJson a

_toWire :: Iso' Result WireResult
_toWire = iso to from where
  to =  case _ of
    Victory playerIndex ->
      WireVictory (playerIndex ^. _WireInt)
    Tie playerIndices ->
      WireTie $ view _WireInt <$> playerIndices
  from = case _ of
    WireVictory wirePlayerIndex ->
      Victory $ wirePlayerIndex .^ _WireInt
    WireTie wirePlayerIndices ->
      Tie $ review _WireInt <$> wirePlayerIndices

data WireResult
  = WireVictory WireInt
  | WireTie (Array WireInt)

derive instance genericWireResult :: Generic WireResult _
derive instance eqWireResult :: Eq WireResult
instance showWireResult :: Show WireResult where
  show result = genericShow result
instance encodeJsonWireResult :: EncodeJson WireResult where
  encodeJson a = genericEncodeJson a
instance decodeJsonWireResult :: DecodeJson WireResult where
  decodeJson a = genericDecodeJson a
instance dynamicByteLengthWireResult
  :: DynamicByteLength WireResult where
  byteLength = genericByteLength
instance encodeArrayBufferWireResult
  :: EncodeArrayBuffer WireResult where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBufferWireResult
  :: DecodeArrayBuffer WireResult where
  readArrayBuffer = genericReadArrayBuffer
