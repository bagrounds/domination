module Domination.Data.Choice where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)

data Choice = TrashUpTo Int (Maybe (Array Int))

derive instance genericChoice :: Generic Choice _
derive instance eqChoice :: Eq Choice
instance showChoice :: Show Choice where show = genericShow
instance encodeJsonChoice :: EncodeJson Choice where
  encodeJson a = genericEncodeJson a
instance decodeJsonChoice :: DecodeJson Choice where
  decodeJson a = genericDecodeJson a

