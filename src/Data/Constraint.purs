--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a set of constraint data types for determining valid moves in card games.
--|
--| ### Key Concepts
--| *   **Constraint Data Type**: A type that represents different types of constraints in a card game.
--| *   **Rule Validation**: The `check` function validates rules based on the given constraint and selected/remaining cards.
--| *   **Generic and Derived Instances**: Derive instances for generic, equality, show, encodeJson, and decodeJson functions for the Constraint data type.
module Domination.Data.Constraint where

import Prim hiding (Constraint)

import Control.Applicative (pure)
import Control.Monad.Error.Class (class MonadError)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (length)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.HeytingAlgebra ((&&), (||))
import Data.Semiring (zero)
import Data.Show (class Show)
import Data.Unit (Unit, unit)
import Relationship (Relationship(..))
import Rule (lengthIs, (<>!), (<@!))
import Rule as Rule

data Constraint
  = UpTo Int
  | Exactly Int
  | DownTo Int
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
      forSelected $ lengthIs LTE n

    DownTo n -> Rule.check $
      forRemaining (lengthIs EQ n)
      ||
      ( forSource (lengthIs LT n)
      && forSelected (lengthIs EQ zero)
      )

    Exactly n -> Rule.check $
      forSelected (lengthIs EQ n)
      ||
      ( forSource (lengthIs LT n)
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
