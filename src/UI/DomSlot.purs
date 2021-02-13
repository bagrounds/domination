module Domination.UI.DomSlot where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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

derive instance genericArea :: Generic Area _
derive instance eqArea :: Eq Area
derive instance ordArea :: Ord Area
instance showArea :: Show Area where
  show = genericShow

