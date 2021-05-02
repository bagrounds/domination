module Domination.Data.StackEvaluation where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Domination.Data.Constraint (Constraint)

data StackExpression
  = StackChooseCardsFromHand Constraint (Maybe (Array Int))
  | StackDuplicate
  | StackDiscard
  | StackLength
  | StackDraw

derive instance genericStackExpression :: Generic StackExpression _
derive instance eqStackExpression :: Eq StackExpression
instance showStackExpression :: Show StackExpression where
  show a = genericShow a
instance encodeJsonStackExpression :: EncodeJson StackExpression where
  encodeJson a = genericEncodeJson a
instance decodeJsonStackExpression :: DecodeJson StackExpression where
  decodeJson a = genericDecodeJson a

data StackValue
  = StackArrayInt (Array Int)
  | StackInt Int

derive instance genericStackValue :: Generic StackValue _
derive instance eqStackValue :: Eq StackValue
instance showStackValue :: Show StackValue where
  show a = genericShow a
instance encodeJsonStackValue :: EncodeJson StackValue where
  encodeJson a = genericEncodeJson a
instance decodeJsonStackValue :: DecodeJson StackValue where
  decodeJson a = genericDecodeJson a

