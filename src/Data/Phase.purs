module Domination.Data.Phase where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Phase
  = ActionPhase
  | BuyPhase
  | CleanupPhase

derive instance genericPhase :: Generic Phase _
derive instance eqPhase :: Eq Phase

instance showPhase :: Show Phase where
  show = genericShow
instance encodeJsonPhase :: EncodeJson Phase where
  encodeJson = genericEncodeJson
instance decodeJsonPhase :: DecodeJson Phase where
  decodeJson = genericDecodeJson

next :: Phase -> Phase
next ActionPhase = BuyPhase
next BuyPhase = CleanupPhase
next CleanupPhase = ActionPhase

