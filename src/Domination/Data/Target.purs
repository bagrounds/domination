module Domination.Data.Target where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Target
  = Self
  | Everyone
  | EveryoneElse

derive instance genericTarget :: Generic Target _
derive instance eqTarget :: Eq Target

instance showTarget :: Show Target where
  show = genericShow
instance encodeJsonTarget :: EncodeJson Target where
  encodeJson = genericEncodeJson
instance decodeJsonTarget :: DecodeJson Target where
  decodeJson = genericDecodeJson

