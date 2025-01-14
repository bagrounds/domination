--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a data type called `Condition` with various possible values, along with associated functions for encoding, decoding, showing, and more.
--|
--| ### Key Concepts
--| *   Data types: `Condition` with various variants
--| *   Encoding and decoding: `EncodeJson`, `DecodeJson`, `encodeJson`, and `decodeJson`
--| *   Generic programming: use of `genericShow`, `genericDecodeJson`, `genericEncodeJson`, and generic instance methods
module Domination.Data.Condition where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Domination.Data.CardType (CardType)
import Domination.Data.Wire.Int (WireInt)

data Condition
  = HasCard String
  | HasCardType CardType
  | HasDiscard
  | DiscardContains String
  | DiscardContainsCardType CardType
  | TrashContainsCardType CardType
  | Randomly WireInt

derive instance genericCondition :: Generic Condition _
derive instance eqCondition :: Eq Condition
instance showCondition :: Show Condition where
  show condition = genericShow condition
instance encodeJsonCondition :: EncodeJson Condition where
  encodeJson a = genericEncodeJson a
instance decodeJsonCondition :: DecodeJson Condition where
  decodeJson a = genericDecodeJson a
instance dynamicByteLengthCondition
  :: DynamicByteLength Condition where
  byteLength = genericByteLength
instance encodeArrayBufferCondition
  :: EncodeArrayBuffer Condition where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBufferCondition
  :: DecodeArrayBuffer Condition where
  readArrayBuffer = genericReadArrayBuffer
