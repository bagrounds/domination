module Domination.Capability.WireCodec where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, decodeArrayBuffer, encodeArrayBuffer)
import Data.Either (Either(..), note)
import Domination.AppM (AppM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
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

newtype WireCodecM a = WireCodecM (Aff a)

derive newtype instance functorWireCodecM :: Functor WireCodecM
derive newtype instance applyWireCodecM :: Apply WireCodecM
derive newtype instance applicativeWireCodecM :: Applicative WireCodecM
derive newtype instance bindWireCodecM :: Bind WireCodecM
derive newtype instance monadWireCodecM :: Monad WireCodecM
derive newtype instance monadEffectWireCodecM :: MonadEffect WireCodecM
derive newtype instance monadAffWireCodecM :: MonadAff WireCodecM

instance wireCodecWireCodecM :: WireCodec WireCodecM where
  writeWire = liftEffect <<< writeWire'
  readWire = liftEffect <<< readWire'

runWireCodecM :: WireCodecM ~> Aff
runWireCodecM (WireCodecM m) = liftAff m

writeWire'
  :: forall a
  . EncodeArrayBuffer a
  => DynamicByteLength a
  => a
  -> Effect (Either String String)
writeWire' = encodeArrayBuffer
  >>> map (FFI.arrayBufferAsString >>> Right)

readWire'
  :: forall a
  . DecodeArrayBuffer a
  => String
  -> Effect (Either String a)
readWire' = FFI.stringAsArrayBuffer >>> decodeArrayBuffer
  >>> map (note "failed to read wire formatted messgae")

