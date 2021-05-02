module Domination.Data.Constraint where

import Control.Applicative (pure)
import Control.Monad.Error.Class (class MonadError)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array (length)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HeytingAlgebra ((&&), (||))
import Data.Lens.Prism (review)
import Data.Semiring (zero)
import Data.Show (class Show)
import Data.Unit (Unit, unit)
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Relation (Relation(..))
import Rule (lengthIs, (<>!), (<@!))
import Rule as Rule

data Constraint
  = UpTo WireInt
  | Exactly WireInt
  | DownTo WireInt
  | Unlimited

check
  :: forall m a
  . MonadError String m
  => Show a
  => Constraint
  -> Array a
  -> Array a
  -> Array a
  -> m Unit
check constraint selected remaining sourcePile =
  let
    forSelected = ("selected cards" <>! _) >>> (selected <@! _)
    forRemaining = ("remaining cards" <>! _) >>> (remaining <@! _)
    forSource = ("source cards" <>! _) >>> (sourcePile <@! _)
  in
  case constraint of
    UpTo n -> Rule.check $
      forSelected $ lengthIs LTE (review Int._toWire n)

    DownTo n -> Rule.check $
      forRemaining (lengthIs EQ $ review Int._toWire n)
      ||
      ( forSource (lengthIs LT $ review Int._toWire n)
      && forSelected (lengthIs EQ zero)
      )

    Exactly n -> Rule.check $
      forSelected (lengthIs EQ $ review Int._toWire n)
      ||
      ( forSource (lengthIs LT $ review Int._toWire n)
      && forSelected (lengthIs EQ $ length sourcePile)
      )

    Unlimited -> pure unit

derive instance genericConstraint :: Generic Constraint _
derive instance eqConstraint :: Eq Constraint

instance showConstraint :: Show Constraint where
  show = genericShow
instance encodeJsonConstraint :: EncodeJson Constraint where
  encodeJson = genericEncodeJson
instance decodeJsonConstraint :: DecodeJson Constraint where
  decodeJson = genericDecodeJson

instance dynamicByteLengthConstraint
  :: DynamicByteLength Constraint where
  byteLength = genericByteLength
instance encodeArrayBuffeConstraint
  :: EncodeArrayBuffer Constraint where
  putArrayBuffer = genericPutArrayBuffer
instance decodeArrayBuffeConstraint
  :: DecodeArrayBuffer Constraint where
  readArrayBuffer = genericReadArrayBuffer

