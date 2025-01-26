--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a data type Var with two variants, Unbound and Bound(a), along with various instance declarations for Argonaut serialization and deserialization.
--|
--| ### Key Concepts
--| * Generic type class for deriving instances
--| * Argonaut library (JSON encoding and decoding)
--| * Array buffer manipulation using generic functions
module Domination.Data.Var where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Var a = Unbound | Bound a

derive instance genericVar :: Generic (Var a) _

derive instance eqVar :: Eq a => Eq (Var a)

derive instance functorVar :: Functor Var

instance showVar :: Show a => Show (Var a) where
  show a = genericShow a

instance encodeJsonVar :: EncodeJson a => EncodeJson (Var a) where
  encodeJson a = genericEncodeJson a

instance decodeJsonVar :: DecodeJson a => DecodeJson (Var a) where
  decodeJson a = genericDecodeJson a

instance dynamicByteLengthVar
  :: DynamicByteLength a => DynamicByteLength (Var a) where
  byteLength x = genericByteLength x

instance encodeArrayBuffeVar
  :: EncodeArrayBuffer a => EncodeArrayBuffer (Var a) where
  putArrayBuffer x = genericPutArrayBuffer x

instance decodeArrayBuffeVar
  :: (DecodeArrayBuffer a, DynamicByteLength a)
  => DecodeArrayBuffer (Var a) where
  readArrayBuffer x = genericReadArrayBuffer x
