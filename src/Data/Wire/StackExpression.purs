module Domination.Data.Wire.StackExpression where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Maybe (Maybe)
import Domination.Data.Constraint (Constraint)
import Domination.Data.StackEvaluation (StackExpression(..))
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int

data WireStackExpression
  = WireStackChooseCardsFromHand Constraint (Maybe (Array WireInt))
  | WireStackDuplicate
  | WireStackDiscard
  | WireStackLength
  | WireStackDraw

_toWire :: Iso' StackExpression WireStackExpression
_toWire = iso to from where
  to = case _ of
    StackChooseCardsFromHand constraint maybeXs ->
      WireStackChooseCardsFromHand
        constraint
        $ map (view Int._toWire) <$> maybeXs
    StackDuplicate -> WireStackDuplicate
    StackDiscard -> WireStackDiscard
    StackLength -> WireStackLength
    StackDraw -> WireStackDraw
  from = case _ of
    WireStackChooseCardsFromHand constraint maybeXs ->
      StackChooseCardsFromHand
        constraint
        $ map (review Int._toWire) <$> maybeXs
    WireStackDuplicate -> StackDuplicate
    WireStackDiscard -> StackDiscard
    WireStackLength -> StackLength
    WireStackDraw -> StackDraw

derive instance genericWireStackExpression
  :: Generic WireStackExpression _
derive instance eqWireStackExpression :: Eq WireStackExpression
instance showWireStackExpression :: Show WireStackExpression where
  show wireChoice = genericShow wireChoice
instance encodeJsonWireStackExpression
  :: EncodeJson WireStackExpression where
  encodeJson a = genericEncodeJson a
instance decodeJsonWireStackExpression
  :: DecodeJson WireStackExpression where
  decodeJson a = genericDecodeJson a
instance dynamicByteLengthWireStackExpression
  :: DynamicByteLength WireStackExpression where
  byteLength x = genericByteLength x
instance encodeArrayBuffeWireStackExpression
  :: EncodeArrayBuffer WireStackExpression where
  putArrayBuffer x = genericPutArrayBuffer x
instance decodeArrayBuffeWireStackExpression
  :: DecodeArrayBuffer WireStackExpression where
  readArrayBuffer x = genericReadArrayBuffer x

