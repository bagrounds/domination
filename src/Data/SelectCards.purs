--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a data type `SelectCards` with various encoding, decoding, and generic instance implementations for it.
--|
--| ### Key Concepts
--| * Argonaut library usage
--| * Generic data types and their instances
--| * JSON encoding and decoding with Argonaut

module Domination.Data.SelectCards where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data SelectCards
  = SelectAll

derive instance genericSelectCards :: Generic SelectCards _
derive instance eqSelectCards :: Eq SelectCards

instance showSelectCards :: Show SelectCards where
  show = genericShow
instance encodeJsonSelectCards :: EncodeJson SelectCards where
  encodeJson = genericEncodeJson
instance decodeJsonSelectCards :: DecodeJson SelectCards where
  decodeJson = genericDecodeJson

instance dynamicByteLengthSelectCards
  :: DynamicByteLength SelectCards where
  byteLength = genericByteLength
instance encodeArrayBuffeSelectCards
  :: EncodeArrayBuffer SelectCards where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeSelectCards
  :: DecodeArrayBuffer SelectCards where
  readArrayBuffer = genericReadArrayBuffer
