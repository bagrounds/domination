module Domination.Data.Wire.Card where

import Prelude

import Data.Array (findIndex, (!!))
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Maybe (fromMaybe)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.Cards (cardMap)
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int

_toWire :: Iso' Card WireInt
_toWire = iso to from where
  to card = view Int._toWire
    <<< fromMaybe (-1) $ findIndex (_ == card) cardMap
  from = fromMaybe Card.card
    <<< (cardMap !! _)
    <<< review Int._toWire

