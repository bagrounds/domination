--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Module defines data type and encoding/decoding functions for "Reaction" in game Domination.
--|
--| ### Key Concepts
--| * Data types and instances for encoding/decoding reactions.
--| * Derivations for genericShow, encodeJson, decodeJson, dynamicByteLength, encodeArrayBuffer, and decodeArrayBuffer.
--| * Generic classes and functions.
module Domination.Data.Reaction where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Reaction
  = BlockAttack

derive instance genericReaction :: Generic Reaction _
derive instance eqReaction :: Eq Reaction
instance showReaction :: Show Reaction where show = genericShow
instance encodeJsonReaction :: EncodeJson Reaction where
  encodeJson a = genericEncodeJson a
instance decodeJsonReaction :: DecodeJson Reaction where
  decodeJson a = genericDecodeJson a
instance dynamicByteLengthReaction :: DynamicByteLength Reaction where
  byteLength = genericByteLength
instance encodeArrayBufferReaction :: EncodeArrayBuffer Reaction where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBufferReaction :: DecodeArrayBuffer Reaction where
  readArrayBuffer = genericReadArrayBuffer
