module Domination.Data.Bonus where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Bonus
  = Cash Int

derive instance genericBonus :: Generic Bonus _
derive instance eqBonus :: Eq Bonus
instance showBonus :: Show Bonus where show = genericShow
instance encodeJsonBonus :: EncodeJson Bonus where
  encodeJson a = genericEncodeJson a
instance decodeJsonBonus :: DecodeJson Bonus where
  decodeJson a = genericDecodeJson a

