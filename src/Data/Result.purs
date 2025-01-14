--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a data type for game results with encoding, decoding, and showing capabilities.
--|
--| ### Key Concepts
--| * Argonaut library for JSON encoding and decoding
--| * Generic data types with derived instances for Show, EncodeJson, DecodeJson
--| * Data type Result representing a game outcome (Victory or Tie) with associated encoding/decoding and equality functions
module Domination.Data.Result where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Result
  = Victory Int
  | Tie (Array Int)

derive instance genericResult :: Generic Result _
derive instance eqResult :: Eq Result
instance showResult :: Show Result where
  show result = genericShow result
instance encodeJsonResult :: EncodeJson Result where
  encodeJson a = genericEncodeJson a
instance decodeJsonResult :: DecodeJson Result where
  decodeJson a = genericDecodeJson a
