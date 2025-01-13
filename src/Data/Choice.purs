--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Data types and functions for representing choices in a card game.
--|
--| ### Key Concepts
--| * `Choice` data type and its various constructors
--| * `isAttack` function to check if a `Choice` is an attack action
--| * `generic*` instances for `Generic`, `Eq`, `Show`, `EncodeJson`, and `DecodeJson` classes applied to the `Choice` type
module Domination.Data.Choice where

import Prim hiding (Constraint)
import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Domination.Data.Actions (Actions)
import Domination.Data.Bonus (Bonus)
import Domination.Data.Buys (Buys)
import Domination.Data.Condition (Condition)
import Domination.Data.Constraint (Constraint)
import Domination.Data.Filter (Filter)
import Domination.Data.Pile (Pile)
import Domination.Data.SelectCards (SelectCards)
import Domination.Data.StackEvaluation (StackExpression, StackValue)

data Choice
  = If
    { choice :: Choice
    , otherwise :: Maybe Choice
    , condition :: Condition
    , resolution :: Maybe Unit
    , attack :: Boolean
    }
  | And
    { choices :: Array Choice
    , resolution :: Maybe Unit
    , attack :: Boolean
    }
  | Or
    { choices :: Array Choice
    , resolution :: Maybe Choice
    , attack :: Boolean
    }
  | PickN
    { choices :: Array Choice
    , n :: Int
    , resolution :: Maybe (Array Choice)
    , attack :: Boolean
    }
  | Option
    { choice :: Choice
    , resolution :: Maybe Boolean
    , attack :: Boolean
    }
  | MoveFromTo
    { n :: Constraint
    , filter :: Filter
    , source :: Pile
    , destination :: Pile
    , resolution :: Maybe (Array Int)
    , attack :: Boolean
    }
  | GainCard
    { attack :: Boolean
    , filter :: Filter
    , destination :: Pile
    , resolution :: Maybe String
    }
  | GainCards
    { attack :: Boolean
    , cardName :: String
    , destination :: Pile
    , n :: Int
    , resolution :: Maybe Unit
    }
  | GainActions
    { n :: Actions
    , resolution :: Maybe Unit
    , attack :: Boolean
    }
  | GainBuys
    { n :: Buys
    , resolution :: Maybe Unit
    , attack :: Boolean
    }
  | Discard
    { selection :: SelectCards
    , resolution :: Maybe Unit
    , attack :: Boolean
    }
  | Draw
    { n :: Int
    , resolution :: Maybe Unit
    , attack :: Boolean
    }
  | GainBonus
    { bonus :: Bonus
    , resolution :: Maybe Unit
    , attack :: Boolean
    }
  | StackChoice
    { expression :: Array StackExpression
    , stack :: Array StackValue
    , attack :: Boolean
    , description :: String
    }

isAttack :: Choice -> Boolean
isAttack = case _ of
  If { attack } -> attack
  And { attack } -> attack
  Or { attack } -> attack
  PickN { attack } -> attack
  Option { attack } -> attack
  MoveFromTo { attack } -> attack
  GainCards { attack } -> attack
  GainCard { attack } -> attack
  GainActions { attack } -> attack
  GainBuys { attack } -> attack
  Discard { attack } -> attack
  Draw { attack } -> attack
  GainBonus { attack } -> attack
  StackChoice { attack } -> attack

derive instance genericChoice :: Generic Choice _

derive instance eqChoice :: Eq Choice

instance showChoice :: Show Choice where
  show choice = genericShow choice

instance encodeJsonChoice :: EncodeJson Choice where
  encodeJson a = genericEncodeJson a

instance decodeJsonChoice :: DecodeJson Choice where
  decodeJson a = genericDecodeJson a
