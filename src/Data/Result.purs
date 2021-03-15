module Domination.Data.Result where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

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

