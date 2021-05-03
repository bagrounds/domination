module Domination.Data.StackEvaluation where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Domination.Data.Constraint (Constraint)
import Domination.Data.Filter (Filter)
import Domination.Data.Pile (Pile)
import Domination.Data.Var (Var)

data StackExpression
  = StackChooseCards
    { cards :: Var (Array Int)
    , filter :: Var Filter
    , from :: Var Pile
    , n :: Var Constraint
    }
  | StackDuplicate
  | StackDiscard
  | StackLength
  | StackAddN Int
  | StackDraw
  | StackTrash
  | StackNth Int
  | StackCostOf
  | StackMakeFilterCostUpTo
  | StackBind String
  | StackGainTo Pile
  | StackGainCard
    { cardName :: Var String
    , filter :: Var Filter
    }

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
  | StackString String
  | StackFilter Filter

derive instance genericStackValue :: Generic StackValue _
derive instance eqStackValue :: Eq StackValue
instance showStackValue :: Show StackValue where
  show a = genericShow a
instance encodeJsonStackValue :: EncodeJson StackValue where
  encodeJson a = genericEncodeJson a
instance decodeJsonStackValue :: DecodeJson StackValue where
  decodeJson a = genericDecodeJson a

