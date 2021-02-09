module Domination.Data.Pile where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Pile
  = Hand
  | Discard
  | Deck
  | Trash

derive instance eqPile :: Eq Pile
derive instance genericPile :: Generic Pile _
instance encodeJsonPile :: EncodeJson Pile where
  encodeJson = genericEncodeJson
instance decodeJsonPile :: DecodeJson Pile where
  decodeJson = genericDecodeJson
instance showPile :: Show Pile where
  show = genericShow

instance dynamicByteLengthPile :: DynamicByteLength Pile where
  byteLength = genericByteLength
instance encodeArrayBuffePile :: EncodeArrayBuffer Pile where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffePile :: DecodeArrayBuffer Pile where
  readArrayBuffer = genericReadArrayBuffer

