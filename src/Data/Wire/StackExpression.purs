module Domination.Data.Wire.StackExpression where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Domination.Data.Pile (Pile)
import Domination.Data.StackEvaluation (StackExpression(..))
import Domination.Data.Var (Var)
import Domination.Data.Wire.Constraint (WireConstraint)
import Domination.Data.Wire.Constraint as Constraint
import Domination.Data.Wire.Filter (WireFilter)
import Domination.Data.Wire.Filter as Filter
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Util ((.^))

data WireStackExpression
  = WireStackChooseCards
    (Var (Array WireInt)) -- cards
    (Var WireFilter)
    (Var Pile)
    (Var WireConstraint)
  | WireStackDuplicate
  | WireStackDiscard
  | WireStackLength
  | WireStackAddN WireInt
  | WireStackDraw
  | WireStackTrash
  | WireStackNth WireInt
  | WireStackCostOf
  | WireStackMakeFilterCostUpTo
  | WireStackBind String
  | WireStackGainTo Pile
  | WireStackGainCard (Var String) (Var WireFilter)

_toWire :: Iso' StackExpression WireStackExpression
_toWire = iso to fro where
  to = case _ of
    StackChooseCards { cards, filter, from, n } ->
      WireStackChooseCards
        (map (view Int._toWire) <$> cards)
        (view Filter._toWire <$> filter)
        from
        (view Constraint._toWire <$> n)
    StackDuplicate -> WireStackDuplicate
    StackDiscard -> WireStackDiscard
    StackLength -> WireStackLength
    StackAddN n -> WireStackAddN $ n ^. Int._toWire
    StackDraw -> WireStackDraw
    StackTrash -> WireStackTrash
    StackNth n -> WireStackNth $ n ^. Int._toWire
    StackCostOf -> WireStackCostOf
    StackMakeFilterCostUpTo -> WireStackMakeFilterCostUpTo
    StackBind s -> WireStackBind s
    StackGainTo pile -> WireStackGainTo pile
    StackGainCard { cardName, filter } ->
      WireStackGainCard cardName (view Filter._toWire <$> filter)
  fro = case _ of
    WireStackChooseCards cards filter from n ->
      StackChooseCards
        { cards: map (review Int._toWire) <$> cards
        , filter: review Filter._toWire <$> filter
        , from
        , n: review Constraint._toWire <$> n
        }
    WireStackDuplicate -> StackDuplicate
    WireStackDiscard -> StackDiscard
    WireStackLength -> StackLength
    WireStackAddN n -> StackAddN $ n .^ Int._toWire
    WireStackDraw -> StackDraw
    WireStackTrash -> StackTrash
    WireStackNth n -> StackNth $ n .^ Int._toWire
    WireStackCostOf -> StackCostOf
    WireStackMakeFilterCostUpTo -> StackMakeFilterCostUpTo
    WireStackBind s -> StackBind s
    WireStackGainTo pile -> StackGainTo pile
    WireStackGainCard cardName filter ->
      StackGainCard
        { cardName
        , filter: review Filter._toWire <$> filter
        }

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

