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
import Data.Show.Generic (genericShow)
import Data.Lens.Iso (Iso', re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (review)
import Data.Newtype (class Newtype)
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

_int :: Iso' Actions Int
_int = _Newtype <<< (re Int._toWire)

