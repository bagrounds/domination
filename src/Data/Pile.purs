{-
  This module defines the 'Pile' data type and related instances for the Domination game.
  It includes various instances for equality, ordering, JSON encoding/decoding, and more.

  Key Components:
  - Pile: Represents different piles in the game (e.g., Supply, Trash, Deck, Hand).
  - Instances: Various instances for equality, ordering, JSON encoding/decoding, etc.

  Technical Concepts:
  * Generic: Automatically derives instances for data types.
  * EncodeJson/DecodeJson: JSON serialization and deserialization.
  * DynamicByteLength: Calculates the byte length of data for binary encoding.
-}
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

