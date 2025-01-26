--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines an enumeration for different targets with associated encoding and decoding instances.
--|
--| ### Key Concepts
--| * Argonaut library for JSON encoding and decoding
--| * Data types and instances related to JSON serialization/deserialization
module Domination.Data.Target where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
