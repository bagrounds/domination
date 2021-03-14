module Domination.Data.Choice where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Domination.Data.Actions (Actions)
import Domination.Data.Bonus (Bonus)
import Domination.Data.Buys (Buys)
import Domination.Data.Condition (Condition)
import Domination.Data.Constraint (Constraint)
import Domination.Data.Filter (Filter)
import Domination.Data.Pile (Pile)
import Domination.Data.SelectCards (SelectCards)

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
    , filter :: Maybe Filter
    , source :: Pile
    , destination :: Pile
    , resolution :: Maybe (Array Int)
    , attack :: Boolean
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

isAttack :: Choice -> Boolean
isAttack = case _ of
  If { attack } -> attack
  And { attack } -> attack
  Or { attack } -> attack
  PickN { attack } -> attack
  Option { attack } -> attack
  MoveFromTo { attack } -> attack
  GainCards { attack } -> attack
  GainActions { attack } -> attack
  GainBuys { attack } -> attack
  Discard { attack } -> attack
  Draw { attack } -> attack
  GainBonus { attack } -> attack

derive instance genericChoice :: Generic Choice _
derive instance eqChoice :: Eq Choice
instance showChoice :: Show Choice where
  show choice = genericShow choice
instance encodeJsonChoice :: EncodeJson Choice where
  encodeJson a = genericEncodeJson a
instance decodeJsonChoice :: DecodeJson Choice where
  decodeJson a = genericDecodeJson a

