module Domination.Data.Wire.Choice where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.ArrayBuffer.Class (class DecodeArrayBuffer, class DynamicByteLength, class EncodeArrayBuffer, genericByteLength, genericPutArrayBuffer, genericReadArrayBuffer)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Fold ((^?))
import Data.Lens.Getter (view, (^.))
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Lens.Prism.Maybe (_Just)
import Data.Maybe (Maybe)
import Domination.Data.Actions (Actions)
import Domination.Data.Bonus (Bonus)
import Domination.Data.Buys (Buys)
import Domination.Data.Choice (Choice(..))
import Domination.Data.Condition (Condition)
import Domination.Data.Constraint (Constraint)
import Domination.Data.Filter (Filter)
import Domination.Data.Pile (Pile)
import Domination.Data.SelectCards (SelectCards)
import Domination.Data.WireInt (WireInt, _WireInt)

data WireChoice
  = WireIf Boolean WireChoice Condition (Maybe WireChoice) (Maybe Unit)
  | WireAnd Boolean (Array WireChoice) (Maybe Unit)
  | WireOr Boolean (Array WireChoice) (Maybe WireChoice)
  | WirePickN Boolean (Array WireChoice) WireInt
    (Maybe (Array WireChoice))
  | WireOption Boolean WireChoice (Maybe Boolean)
  | WireMoveFromTo Boolean Pile (Maybe Filter) Constraint
    (Maybe (Array WireInt)) Pile
  | WireGainCards Boolean String Pile WireInt (Maybe Unit)
  | WireGainCard Boolean Pile (Maybe Filter) (Maybe String)
  | WireGainActions Boolean Actions (Maybe Unit)
  | WireGainBuys Boolean Buys (Maybe Unit)
  | WireDiscard Boolean (Maybe Unit) SelectCards
  | WireDraw Boolean WireInt (Maybe Unit)
  | WireGainBonus Boolean Bonus (Maybe Unit)

_toWire :: Iso' Choice WireChoice
_toWire = iso to from where
  to =  case _ of
    If { attack, choice, condition, otherwise, resolution } ->
      WireIf attack (choice ^. _toWire) condition
        (otherwise ^? (_Just <<< _toWire)) resolution
    And { attack, choices, resolution } ->
      WireAnd attack ((_ ^. _toWire) <$> choices) resolution
    Or { attack, choices, resolution } ->
      WireOr attack ((_ ^. _toWire) <$> choices) (resolution ^? (_Just <<< _toWire))
    PickN { attack, choices, n, resolution } ->
      WirePickN attack ((_ ^. _toWire) <$> choices) (n ^. _WireInt)
        (map (_ ^. _toWire) <$> resolution)
    Option { attack, choice, resolution } ->
      WireOption attack (choice ^. _toWire) resolution
    MoveFromTo { attack, destination, filter, n, resolution, source } ->
      WireMoveFromTo attack destination filter n ((map $ view _WireInt) <$> resolution) source
    GainCards { attack, cardName, destination, n, resolution } ->
      WireGainCards attack cardName destination (n ^. _WireInt) resolution
    GainCard { attack, destination, filter, resolution } ->
      WireGainCard attack destination filter resolution
    GainActions { attack, n, resolution } ->
      WireGainActions attack n resolution
    GainBuys { attack, n, resolution } ->
      WireGainBuys attack n resolution
    Discard { attack, resolution, selection } ->
      WireDiscard attack resolution selection
    Draw { attack, n, resolution } ->
      WireDraw attack (n ^. _WireInt) resolution
    GainBonus { attack, bonus, resolution } ->
      WireGainBonus attack bonus resolution
  from = case _ of
    WireIf attack choice condition otherwise resolution ->
      If { attack, choice: review _toWire choice, condition, otherwise: review _toWire <$> otherwise, resolution }
    WireAnd attack choices resolution ->
      And { attack, choices: review _toWire <$> choices, resolution }
    WireOr attack choices resolution ->
      Or { attack, choices: review _toWire <$> choices, resolution: review _toWire <$> resolution }
    WirePickN attack choices n resolution ->
      PickN { attack, choices: review _toWire <$> choices, n: review _WireInt n, resolution: (map $ review _toWire) <$> resolution }
    WireOption attack choice resolution ->
      Option { attack, choice: review _toWire choice, resolution }
    WireMoveFromTo attack destination filter n resolution source ->
      MoveFromTo { attack, destination, filter, n, resolution: (map $ review _WireInt) <$> resolution, source }
    WireGainCards attack cardName destination n resolution ->
      GainCards { attack, cardName, destination, n: review _WireInt n, resolution }
    WireGainCard attack destination filter resolution ->
      GainCard { attack, destination, filter, resolution }
    WireGainActions attack n resolution ->
      GainActions { attack, n, resolution }
    WireGainBuys attack n resolution ->
      GainBuys { attack, n, resolution }
    WireDiscard attack resolution selection ->
      Discard { attack, resolution, selection }
    WireDraw attack n resolution ->
      Draw { attack, n: review _WireInt n, resolution }
    WireGainBonus attack bonus resolution ->
      GainBonus { attack, bonus, resolution }

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

