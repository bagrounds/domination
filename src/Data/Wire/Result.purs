--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Provides wire format for a Result data type with variants Victory and Tie, along with various encoding and decoding mechanisms.
--|
--| ### Key Concepts
--| * Iso transformation for mapping `Result` to `WireResult`
--| * Deriving instances of generic types (`Generic`, `Eq`, `Show`, `EncodeJson`, `DecodeJson`) for `WireResult`
--| * Using generic functions for encoding and decoding array buffers (`dynamicByteLength`, `encodeArrayBuffer`, `decodeArrayBuffer`)
module Domination.Data.Wire.Result where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Domination.Data.Result (Result(..))
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Util ((.^))

_toWire :: Iso' Result WireResult
_toWire = iso to from where
  to =  case _ of
    Victory playerIndex ->
      WireVictory (playerIndex ^. Int._toWire)
    Tie playerIndices ->
      WireTie $ view Int._toWire <$> playerIndices
  from = case _ of
    WireVictory wirePlayerIndex ->
      Victory $ wirePlayerIndex .^ Int._toWire
    WireTie wirePlayerIndices ->
      Tie $ review Int._toWire <$> wirePlayerIndices

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
