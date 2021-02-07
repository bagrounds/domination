module Domination.Data.WireInt where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer)
import Data.ArrayBuffer.Class.Types (Int8(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Iso (Iso', iso)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype WireInt = WireInt Int8

derive instance genericWireInt :: Generic WireInt _
derive instance eqWireInt :: Eq WireInt
derive instance ordWireInt :: Ord WireInt
instance showWireInt :: Show WireInt where show = genericShow
instance encodeJsonWireInt :: EncodeJson WireInt where
  encodeJson (WireInt (Int8 i)) = encodeJson i
instance decodeJsonWireInt :: DecodeJson WireInt where
  decodeJson i = (WireInt <<< Int8) <$> decodeJson i
derive newtype instance encodeArrayBufferWireInt
  :: EncodeArrayBuffer WireInt
derive newtype instance decodeArrayBufferWireInt
  :: DecodeArrayBuffer WireInt
derive newtype instance dynamicByteLengthWireInt
  :: DynamicByteLength WireInt
instance arbitraryWireInt :: Arbitrary WireInt where
  arbitrary = (WireInt <<< Int8 <<< (_ `mod` 256)) <$> arbitrary

_WireInt :: Iso' Int WireInt
_WireInt = iso (WireInt <<< Int8) $ \(WireInt (Int8 i)) -> i

