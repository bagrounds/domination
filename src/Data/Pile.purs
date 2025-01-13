--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Module defines and derives instances for a data type representing player piles in a card game.
--|
--| ### Key Concepts
--| * The `Pile` data type and its associated types and instances.
--| * The use of Argonaut for encoding, decoding, and showing JSON.
--| * The array buffer support for serializing and deserializing the `Pile` type.
module Domination.Data.Pile where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum, fromEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Show.Generic (genericShow)

data Pile
  = Supply
  | Trash

  | AtPlay
  | Buying
  | Deck
  | Discard
  | Discarding
  | Hand

derive instance eqPile :: Eq Pile

derive instance ordPile :: Ord Pile

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

instance boundedPile :: Bounded Pile where
  bottom = genericBottom
  top = genericTop

instance enumPile :: Enum Pile where
  succ = genericSucc
  pred = genericPred

instance boundedEnum :: BoundedEnum Pile where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance hashablePile :: Hashable Pile where
  hash = fromEnum

playerPiles :: Array Pile
playerPiles =
  [ AtPlay
  , Buying
  , Deck
  , Discard
  , Discarding
  , Hand
  ]
