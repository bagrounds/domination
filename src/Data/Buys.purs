--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Module defines a newtype `Buys` with a single field of type `WireInt`.
--|
--| ### Key Concepts
--| * **Data Types and Derived Instances**: Understanding the `Buys` data type, its derived instances such as `Eq`, `Ord`, `Semiring`, `Ring`, `Generic`, `Show`, `EncodeJson`, `DecodeJson`, and their respective implementations.
--|  
--| * **Encoding and Decoding**: Knowing how `genericEncodeJson` and `genericDecodeJson` work with the `Buys` data type, and understanding the roles of the derived instances in encoding/decoding.
--|
--| * **Lens and Newtype Functions**: Understanding the usage of `_Newtype`, `review`, and `re` functions to define and manipulate data types.
module Domination.Data.Buys
  ( Buys(..)
  , buys
  , _int
  ) where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Lens.Iso (Iso', re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (review)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int

newtype Buys = Buys WireInt

buys :: Int -> Buys
buys = review _int

derive newtype instance eqBuys :: Eq Buys

derive newtype instance ordBuys :: Ord Buys

derive newtype instance ringBuys :: Semiring Buys

derive newtype instance semiringBuys :: Ring Buys

derive instance genericBuys :: Generic Buys _

derive instance newtypeBuys :: Newtype Buys _

instance showBuys :: Show Buys where
  show = genericShow

instance encodeJsonBuys :: EncodeJson Buys where
  encodeJson = genericEncodeJson

instance decodeJsonBuys :: DecodeJson Buys where
  decodeJson = genericDecodeJson

derive newtype instance encodeArrayBufferBuys
  :: EncodeArrayBuffer Buys

derive newtype instance decodeArrayBufferBuys
  :: DecodeArrayBuffer Buys

derive newtype instance dynamicByteLengthBuys
  :: DynamicByteLength Buys

derive newtype instance hashableBuys :: Hashable Buys

_int :: Iso' Buys Int
_int = _Newtype <<< (re Int._toWire)
