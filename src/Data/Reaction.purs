module Domination.Data.Reaction where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Reaction
  = BlockAttack

derive instance genericReaction :: Generic Reaction _
derive instance eqReaction :: Eq Reaction
instance showReaction :: Show Reaction where show = genericShow
instance encodeJsonReaction :: EncodeJson Reaction where
  encodeJson a = genericEncodeJson a
instance decodeJsonReaction :: DecodeJson Reaction where
  decodeJson a = genericDecodeJson a

