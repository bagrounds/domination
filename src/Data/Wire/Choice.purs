--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines an ISO type and its associated data structures for wire protocol choices.
--|
--| ### Key Concepts
--| * **Generic Programming**: This module demonstrates how to use Haskell's generic programming techniques to encode and decode complex data structures.
--| * **Iso and Prism**: The module uses the `Iso'` and `Prism` types from the `Data.Lens` library to define a conversion between a generic data structure (`Choice`) and a specific wire format (`WireChoice`).
--| * **Deriving Generic Instances**: The module shows how to derive generic instances for common Haskell types (e.g. `Generic`, `Eq`, `Show`, `EncodeJson`, etc.) using the `generic` function from the `Data.Generic.Rep` library.
module Domination.Data.Wire.Choice where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens.Fold ((^?))
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Lens.Prism.Maybe (_Just)
import Data.Maybe (Maybe)
import Domination.Data.Actions (Actions)
import Domination.Data.Buys (Buys)
import Domination.Data.Choice (Choice(..))
import Domination.Data.Condition (Condition)
import Domination.Data.Pile (Pile)
import Domination.Data.SelectCards (SelectCards)
import Domination.Data.Wire.Bonus (WireBonus)
import Domination.Data.Wire.Bonus as Bonus
import Domination.Data.Wire.Constraint (WireConstraint)
import Domination.Data.Wire.Constraint as Constraint
import Domination.Data.Wire.Filter (WireFilter)
import Domination.Data.Wire.Filter as Filter
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Domination.Data.Wire.StackExpression (WireStackExpression)
import Domination.Data.Wire.StackExpression as StackExpression
import Domination.Data.Wire.StackValue (WireStackValue)
import Domination.Data.Wire.StackValue as StackValue
import Util ((.^))

data WireChoice
  = WireIf
    Boolean
    WireChoice
    Condition
    (Maybe WireChoice)
    (Maybe Unit)
  | WireAnd Boolean (Array WireChoice) (Maybe Unit)
  | WireOr Boolean (Array WireChoice) (Maybe WireChoice)
  | WirePickN Boolean (Array WireChoice) WireInt
    (Maybe (Array WireChoice))
  | WireOption Boolean WireChoice (Maybe Boolean)
  | WireMoveFromTo Boolean Pile WireFilter WireConstraint
    (Maybe (Array WireInt)) Pile
  | WireGainCards Boolean String Pile WireInt (Maybe Unit)
  | WireGainCard Boolean Pile WireFilter (Maybe String)
  | WireGainActions Boolean Actions (Maybe Unit)
  | WireGainBuys Boolean Buys (Maybe Unit)
  | WireDiscard Boolean (Maybe Unit) SelectCards
  | WireDraw Boolean WireInt (Maybe Unit)
  | WireGainBonus Boolean WireBonus (Maybe Unit)
  | WireStackChoice
    Boolean
    (Array WireStackExpression)
    (Array WireStackValue)
    String

_toWire :: Iso' Choice WireChoice
_toWire = iso to from where
  to =  case _ of
    If { attack, choice, condition, otherwise, resolution } ->
      WireIf attack (choice ^. _toWire) condition
        (otherwise ^? (_Just <<< _toWire)) resolution

    And { attack, choices, resolution } ->
      WireAnd attack ((_ ^. _toWire) <$> choices) resolution

    Or { attack, choices, resolution } ->
      WireOr
        attack
        ((_ ^. _toWire) <$> choices)
        (resolution ^? (_Just <<< _toWire))

    PickN { attack, choices, n, resolution } ->
      WirePickN
        attack
        ((_ ^. _toWire) <$> choices)
        (n ^. Int._toWire)
        (map (_ ^. _toWire) <$> resolution)

    Option { attack, choice, resolution } ->
      WireOption attack (choice ^. _toWire) resolution

    MoveFromTo
      { attack, destination, filter, n, resolution, source } ->
      WireMoveFromTo
        attack
        destination
        (filter ^. Filter._toWire)
        (n ^. Constraint._toWire)
        ((map $ view Int._toWire) <$> resolution)
        source

    GainCards { attack, cardName, destination, n, resolution } ->
      WireGainCards
        attack
        cardName
        destination
        (n ^. Int._toWire)
        resolution

    GainCard { attack, destination, filter, resolution } ->
      WireGainCard
        attack
        destination
        (filter ^. Filter._toWire)
        resolution

    GainActions { attack, n, resolution } ->
      WireGainActions attack n resolution

    GainBuys { attack, n, resolution } ->
      WireGainBuys attack n resolution

    Discard { attack, resolution, selection } ->
      WireDiscard attack resolution selection

    Draw { attack, n, resolution } ->
      WireDraw attack (n ^. Int._toWire) resolution

    GainBonus { attack, bonus, resolution } ->
      WireGainBonus attack (bonus ^. Bonus._toWire) resolution

    StackChoice { attack, expression, stack, description } ->
      WireStackChoice
        attack
        (view StackExpression._toWire <$> expression)
        (view StackValue._toWire <$> stack)
        description

  from = case _ of
    WireIf attack choice condition otherwise resolution ->
      If
        { attack
        , choice: review _toWire choice
        , condition, otherwise: review _toWire <$> otherwise
        , resolution
        }

    WireAnd attack choices resolution ->
      And { attack, choices: review _toWire <$> choices, resolution }

    WireOr attack choices resolution ->
      Or
        { attack
        , choices: review _toWire <$> choices
        , resolution: review _toWire <$> resolution
        }

    WirePickN attack choices n resolution ->
      PickN
        { attack
        , choices: review _toWire <$> choices
        , n: review Int._toWire n
        , resolution: (map $ review _toWire) <$> resolution
        }

    WireOption attack choice resolution ->
      Option { attack, choice: review _toWire choice, resolution }

    WireMoveFromTo attack destination filter n resolution source ->
      MoveFromTo
        { attack
        , destination
        , filter: filter .^ Filter._toWire
        , n: n .^ Constraint._toWire
        , resolution: (map $ review Int._toWire) <$> resolution
        , source
        }

    WireGainCards attack cardName destination n resolution ->
      GainCards
        { attack
        , cardName
        , destination
        , n: review Int._toWire n, resolution
        }

    WireGainCard attack destination filter resolution ->
      GainCard
        { attack
        , destination
        , filter: filter .^ Filter._toWire
        , resolution
        }

    WireGainActions attack n resolution ->
      GainActions { attack, n, resolution }

    WireGainBuys attack n resolution ->
      GainBuys { attack, n, resolution }

    WireDiscard attack resolution selection ->
      Discard { attack, resolution, selection }

    WireDraw attack n resolution ->
      Draw { attack, n: review Int._toWire n, resolution }

    WireGainBonus attack bonus resolution ->
      GainBonus { attack, bonus: bonus .^ Bonus._toWire, resolution }

    WireStackChoice attack expression stack description ->
      StackChoice
        { attack
        , expression: review StackExpression._toWire <$> expression
        , stack: review StackValue._toWire <$> stack
        , description
        }

derive instance genericWireChoice :: Generic WireChoice _
derive instance eqWireChoice :: Eq WireChoice
instance showWireChoice :: Show WireChoice where
  show wireChoice = genericShow wireChoice
instance encodeJsonWireChoice :: EncodeJson WireChoice where
  encodeJson a = genericEncodeJson a
instance decodeJsonWireChoice :: DecodeJson WireChoice where
  decodeJson a = genericDecodeJson a
instance dynamicByteLengthWireChoice
  :: DynamicByteLength WireChoice where
  byteLength x = genericByteLength x
instance encodeArrayBuffeWireChoice
  :: EncodeArrayBuffer WireChoice where
  putArrayBuffer x = genericPutArrayBuffer x
instance decodeArrayBuffeWireChoice
  :: DecodeArrayBuffer WireChoice where
  readArrayBuffer x = genericReadArrayBuffer x
