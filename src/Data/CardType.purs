--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a data type for card types in a game.
--|
--| ### Key Concepts
--| * Generic data type for representing card types in the Domination game.
--| * Support for JSON encoding and decoding, as well as array buffer encoding and decoding.
--| * Deriving instances for various Argonaut classes.
module Domination.Data.CardType where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data CardType
  = Action
  | Treasure
  | Victory
  | Curse
  | Attack
  | Reaction

derive instance genericCardType :: Generic CardType _
derive instance eqCardType :: Eq CardType

instance showCardType :: Show CardType where
  show = genericShow
instance encodeJsonCardType :: EncodeJson CardType where
  encodeJson = genericEncodeJson
instance decodeJsonCardType :: DecodeJson CardType where
  decodeJson = genericDecodeJson
instance dynamicByteLengthCardType :: DynamicByteLength CardType where
  byteLength = genericByteLength
instance encodeArrayBuffeCardType :: EncodeArrayBuffer CardType where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeCardType :: DecodeArrayBuffer CardType where
  readArrayBuffer = genericReadArrayBuffer
