module Domination.Data.SelectCards where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

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

