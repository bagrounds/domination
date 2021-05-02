module Domination.Data.Filter where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Domination.Data.CardType (CardType)

data Filter
  = HasName String
  | HasType CardType
  | CostUpTo Int

derive instance genericFilter :: Generic Filter _
derive instance eqFilter :: Eq Filter
instance showFilter :: Show Filter where
  show condition = genericShow condition
instance encodeJsonFilter :: EncodeJson Filter where
  encodeJson a = genericEncodeJson a
instance decodeJsonFilter :: DecodeJson Filter where
  decodeJson a = genericDecodeJson a

