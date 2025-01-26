--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a new data type 'Actions' as an instance of WireInt with additional encoding, decoding and showing capabilities.
--|
--| ### Key Concepts
--| * Actions: A newtype data structure wrapping a `WireInt` value.
--| * Generic programming: Various deriving instances for generic functions like `Eq`, `Ord`, `Show`, and more.
--| * Argonaut encoding and decoding: Instances for serializing and deserializing `Actions` values to and from JSON.
module Domination.Data.Actions
  ( Actions(..)
  , actions
  , _int
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Lens.Iso (Iso', re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (review)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int

newtype Actions = Actions WireInt

actions :: Int -> Actions
actions = review _int

derive newtype instance eqActions :: Eq Actions

derive newtype instance ordActions :: Ord Actions

derive newtype instance ringActions :: Semiring Actions

derive newtype instance semiringActions :: Ring Actions

derive instance genericActions :: Generic Actions _

derive instance newtypeActions :: Newtype Actions _

instance showActions :: Show Actions where
  show = genericShow

instance encodeJsonActions :: EncodeJson Actions where
  encodeJson = genericEncodeJson

instance decodeJsonActions :: DecodeJson Actions where
  decodeJson = genericDecodeJson

derive newtype instance encodeArrayBufferActions
  :: EncodeArrayBuffer Actions

derive newtype instance decodeArrayBufferActions
  :: DecodeArrayBuffer Actions

derive newtype instance dynamicByteLengthActions
  :: DynamicByteLength Actions

derive newtype instance hashableActions :: Hashable Actions

_int :: Iso' Actions Int
_int = _Newtype <<< (re Int._toWire)
