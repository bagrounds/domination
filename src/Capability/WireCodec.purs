--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a WireCodec typeclass for encoding/decoding data over wires, using FFI for compression/decompression.
--|
--| ### Key Concepts
--| * **Monad**: A monad is a way to compose effects in a pure functional programming context.
--| * **WireCodecM**: WireCodecM is the type for this module, which represents a wire codec effect that can be used with Halogen and AppM.
--| * **Halogen and AppM Integration**: The module provides instances of `WireCodec` for Halogen (`wireCodecHalogenM`) and AppM (`wireCodecAppM`) to enable using wire codec effects with these state management systems.
module Domination.Capability.WireCodec where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, decodeArrayBuffer, encodeArrayBuffer)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Domination.AppM (AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FFI as FFI
import Halogen (HalogenM)

class Monad m <= WireCodec m where
  writeWire
    :: forall a
    . EncodeArrayBuffer a
    => DynamicByteLength a
    => a
    -> m (Either String String)
  readWire
    :: forall a
    . DecodeArrayBuffer a
    => String
    -> m (Either String a)

instance wireCodecHalogenM
  :: WireCodec m
  => WireCodec (HalogenM st act slots msg m) where
  writeWire = lift <<< writeWire
  readWire = lift <<< readWire

instance wireCodecAppM :: WireCodec AppM where
  writeWire = liftEffect <<< writeWire'
  readWire = liftEffect <<< readWire'

newtype WireCodecM a = WireCodecM (Effect a)

derive newtype instance functorWireCodecM :: Functor WireCodecM
derive newtype instance applyWireCodecM :: Apply WireCodecM
derive newtype instance applicativeWireCodecM :: Applicative WireCodecM
derive newtype instance bindWireCodecM :: Bind WireCodecM
derive newtype instance monadWireCodecM :: Monad WireCodecM
derive newtype instance monadEffectWireCodecM :: MonadEffect WireCodecM

instance wireCodecWireCodecM :: WireCodec WireCodecM where
  writeWire = liftEffect <<< writeWire'
  readWire = liftEffect <<< readWire'

runWireCodecM :: WireCodecM ~> Effect
runWireCodecM (WireCodecM m) = liftEffect m

writeWire'
  :: forall a
  . EncodeArrayBuffer a
  => DynamicByteLength a
  => a
  -> Effect (Either String String)
writeWire' = encodeArrayBuffer
  >>> map (FFI.arrayBufferAsString >>> FFI.compressString >>> Right)

readWire'
  :: forall a
  . DecodeArrayBuffer a
  => String
  -> Effect (Either String a)
readWire' string =
  case FFI.decompressString string of
    Nothing ->
      pure $ Left $ "Failed to decompress string"
        <> " (are you sure this string was compressed?): " <> string
    Just decompressed -> do
      maybeA <- decodeArrayBuffer
        $ FFI.stringAsArrayBuffer decompressed
      pure $ note "Failed to decode ArrayBuffer" maybeA
