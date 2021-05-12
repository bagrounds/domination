module Domination.Data.Reaction where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Domination.Data.Choice (Choice)

data Reaction
  = BlockAttack
  | ReactWithChoice Choice

derive instance genericReaction :: Generic Reaction _

derive instance eqReaction :: Eq Reaction

instance showReaction :: Show Reaction where
  show = genericShow

instance encodeJsonReaction :: EncodeJson Reaction where
  encodeJson = genericEncodeJson

instance decodeJsonReaction :: DecodeJson Reaction where
  decodeJson = genericDecodeJson

