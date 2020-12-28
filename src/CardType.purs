module CardType
  ( CardType(..)
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data CardType = Action | Treasure | Victory | Curse | Attack

derive instance genericCardType :: Generic CardType _
derive instance eqCardType :: Eq CardType
instance showCardType :: Show CardType where show = genericShow
instance encodeJsonCardType :: EncodeJson CardType where
  encodeJson a = genericEncodeJson a
instance decodeJsonCardType :: DecodeJson CardType where
  decodeJson a = genericDecodeJson a

