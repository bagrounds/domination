{-|
Module      : Domination.Data.SelectCards
Description : Data type for card selection operations in a card game

This module provides the SelectCards data type which represents different ways
of selecting cards in a card game. Currently implements:

* SelectAll - Selects all available cards

The type implements standard type class instances including:
* Generic (for deriving instances)
* Eq (for equality comparison)
* Show (for string representation)
* EncodeJson/DecodeJson (for JSON serialization)
* DynamicByteLength/EncodeArrayBuffer/DecodeArrayBuffer (for binary serialization)
-}

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
--  | SelectOne Int
--  | SelectSome (Array Int)
--  | SelectNone

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

