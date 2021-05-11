module Domination.Data.Phase where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
instance dynamicByteLengthPhase :: DynamicByteLength Phase where
  byteLength = genericByteLength
instance phaseEncodeArrayBuffer :: EncodeArrayBuffer Phase where
  putArrayBuffer = genericPutArrayBuffer
instance phaseDecodeArrayBuffer :: DecodeArrayBuffer Phase where
  readArrayBuffer = genericReadArrayBuffer

next :: Phase -> Phase
next ActionPhase = BuyPhase
next BuyPhase = CleanupPhase
next CleanupPhase = ActionPhase

