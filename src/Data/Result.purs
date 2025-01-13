--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A PureScript module for working with a "Result" data type that can be either a Victory (with an integer value) or a Tie (with an array of integers).
--|
--| ### Key Concepts
--| * Deriving Generic and Eq instances for Result
--| * Defining data type Result with Victory and Tie variants
--| * Implementing EncodeJson, DecodeJson, Show, and Eq instances for Result
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
