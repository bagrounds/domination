module Domination.Data.Condition where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Condition
  = HasCard String
  | HasDiscard

derive instance genericCondition :: Generic Condition _
derive instance eqCondition :: Eq Condition
instance showCondition :: Show Condition where
  show condition = genericShow condition
instance encodeJsonCondition :: EncodeJson Condition where
  encodeJson a = genericEncodeJson a
instance decodeJsonCondition :: DecodeJson Condition where
  decodeJson a = genericDecodeJson a
instance dynamicByteLengthCondition :: DynamicByteLength Condition where
  byteLength = genericByteLength
instance encodeArrayBufferCondition :: EncodeArrayBuffer Condition where
  putArrayBuffer = genericPutArrayBuffer

