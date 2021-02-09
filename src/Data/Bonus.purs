module Domination.Data.Bonus where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array (foldr)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.ArrayBuffer.Class.Types (Int8(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Domination.Data.WireInt (WireInt(..))

data Bonus
  = Cash WireInt

cashValue :: Array Bonus -> Int
cashValue = foldr (\a b -> cashValue1 a + b) 0

cashValue1 :: Bonus -> Int
cashValue1 = case _ of
  Cash (WireInt (Int8 c)) -> c

derive instance genericBonus :: Generic Bonus _
derive instance eqBonus :: Eq Bonus
instance showBonus :: Show Bonus where show = genericShow
instance encodeJsonBonus :: EncodeJson Bonus where
  encodeJson a = genericEncodeJson a
instance decodeJsonBonus :: DecodeJson Bonus where
  decodeJson a = genericDecodeJson a

instance dynamicByteLengthBonus :: DynamicByteLength Bonus where
  byteLength = genericByteLength
instance encodeArrayBuffeBonus :: EncodeArrayBuffer Bonus where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeBonus :: DecodeArrayBuffer Bonus where
  readArrayBuffer = genericReadArrayBuffer

