module Domination.Data.Var where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

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

