{-
  This module defines the 'Points' newtype, which represents points in the game.
  It includes functions to create 'Points' from an 'Int' and an ISO for converting
  between 'Points' and 'Int'. It also derives various instances for 'Points' to
  support JSON encoding/decoding, array buffer encoding/decoding, and more.

  Key Components:
  - Points: Represents points in the game.
  - Functions: Functions to create and manipulate points.
  - Instances: Various instances for JSON encoding/decoding, array buffer encoding/decoding, etc.

  Technical Concepts:
  * Newtype: A wrapper around an existing type to create a distinct type.
  * Iso: An isomorphism between two types.
  * JSON Encoding/Decoding: Serialization and deserialization of points.
-}
module Domination.Data.Points
  ( Points(..)
  , points
  , _int
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens.Iso (Iso', re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (review)
import Data.Newtype (class Newtype)
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int

newtype Points = Points WireInt

points :: Int -> Points
points = review _int

derive newtype instance eqPoints :: Eq Points
derive newtype instance ordPoints :: Ord Points
derive newtype instance ringPoints :: Semiring Points
derive newtype instance semiringPoints :: Ring Points
derive instance genericPoints :: Generic Points _
derive instance newtypePoints :: Newtype Points _
instance showPoints :: Show Points where
  show = genericShow
instance encodeJsonPoints :: EncodeJson Points where
  encodeJson = genericEncodeJson
instance decodeJsonPoints :: DecodeJson Points where
  decodeJson = genericDecodeJson
derive newtype instance encodeArrayBufferPoints
  :: EncodeArrayBuffer Points
derive newtype instance decodeArrayBufferPoints
  :: DecodeArrayBuffer Points
derive newtype instance dynamicByteLengthPoints
  :: DynamicByteLength Points

_int :: Iso' Points Int
_int = _Newtype <<< (re Int._toWire)

