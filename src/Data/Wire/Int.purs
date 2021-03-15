module Domination.Data.Wire.Int where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer)
import Data.ArrayBuffer.Class.Types (Int16LE(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Iso (Iso', iso)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype WireInt = WireInt Int16LE

derive instance genericWireInt :: Generic WireInt _
derive instance eqWireInt :: Eq WireInt
derive instance ordWireInt :: Ord WireInt
derive newtype instance semiringWireInt
  :: Semiring WireInt
derive newtype instance ringWireInt
  :: Ring WireInt
instance showWireInt :: Show WireInt where show = genericShow
instance encodeJsonWireInt :: EncodeJson WireInt where
  encodeJson (WireInt (Int16LE i)) = encodeJson i
instance decodeJsonWireInt :: DecodeJson WireInt where
  decodeJson i = (WireInt <<< Int16LE) <$> decodeJson i
derive newtype instance encodeArrayBufferWireInt
  :: EncodeArrayBuffer WireInt
derive newtype instance decodeArrayBufferWireInt
  :: DecodeArrayBuffer WireInt
derive newtype instance dynamicByteLengthWireInt
  :: DynamicByteLength WireInt
instance arbitraryWireInt :: Arbitrary WireInt where
  arbitrary = (WireInt <<< Int16LE <<< (_ `mod` 256)) <$> arbitrary

_toWire :: Iso' Int WireInt
_toWire = iso (WireInt <<< Int16LE) $ \(WireInt (Int16LE i)) -> i

