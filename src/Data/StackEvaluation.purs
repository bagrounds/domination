--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines data types for stack expressions and values used in a card game, along with encoding and decoding functionality.
--|
--| ### Key Concepts
--| * **Stack Expressions**: The core data structure of the module, representing different operations on a stack.
--| * **Stack Values**: The possible values that can be stored or manipulated in a stack, including arrays, integers, strings, filters, and booleans.
--| * **Generic Encoding and Decoding**: The use of generic functions to encode and decode instances of `StackExpression` and `StackValue`, allowing for flexible serialization and deserialization.
module Domination.Data.StackEvaluation where

import Prelude
import Prim hiding (Constraint)

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Domination.Data.Bonus (Bonus)
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
  | StackChooseCardFromSupply
    { cardName :: Var String
    , filter :: Var Filter
    }
  | StackOption (Var Boolean)
  | StackMoveCards { from :: Pile, to :: Pile }
  | StackDiscard
  | StackTrash
  | StackDraw
  | StackGainTo Pile
  | StackGainBonusCash
  | StackDuplicate
  | StackPush StackValue
  | StackLength
  | StackAddN Int
  | StackNth Int
  | StackCostOf
  | StackMakeFilterCostUpTo
  | StackMakeFilterAnd
  | StackEquals StackValue
  | StackBind String
  | StackIf
    { condition :: Array StackExpression
    , following :: Array StackExpression
    , otherwise :: Array StackExpression
    }
  | StackGainBonus Bonus

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
  | StackBool Boolean

derive instance genericStackValue :: Generic StackValue _
derive instance eqStackValue :: Eq StackValue
instance showStackValue :: Show StackValue where
  show a = genericShow a
instance encodeJsonStackValue :: EncodeJson StackValue where
  encodeJson a = genericEncodeJson a
instance decodeJsonStackValue :: DecodeJson StackValue where
  decodeJson a = genericDecodeJson a
