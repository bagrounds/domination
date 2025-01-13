--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines data types for slots on a game board and areas within those slots.
--|
--| ### Key Concepts
--| * `DomSlot` and `Area` are data types representing a DOM slot and areas, respectively.
--| * `genericShow` is used to derive a `Show` instance for both types, which allows them to be converted into strings.
--| * The `DeriveGeneric` pragma automatically derives generic instances for both types, including `Eq`, `Ord`, and `Generic`.
module Domination.UI.DomSlot where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Ord (class Ord)
import Prelude (class Show)

data DomSlot
  = AreaSlot Area
  | CardSlot Area Int

derive instance genericDomSlot :: Generic DomSlot _
derive instance eqDomSlot :: Eq DomSlot
derive instance ordDomSlot :: Ord DomSlot
instance showDomSlot :: Show DomSlot where
  show = genericShow

data Area
  = GameArea
  | SupplyArea
  | AtPlayArea
  | BuyingArea
  | HandArea
  | ChoiceArea
  | KingdomConfigArea

derive instance genericArea :: Generic Area _
derive instance eqArea :: Eq Area
derive instance ordArea :: Ord Area
instance showArea :: Show Area where
  show = genericShow
