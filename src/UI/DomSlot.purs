--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines data types for game areas and slots.
--|
--| ### Key Concepts
--| * DomSlot and its variants (AreaSlot, CardSlot)
--| * Area types
--| * Generic and derived instances for DomSlot and Area
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
