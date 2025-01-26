--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines an Iso type for converting between a Card and its corresponding WireInt value.
--|
--| ### Key Concepts
--| * Iso' and Prism
--| * Lens (Getter and Iso')
--| * Data mapping between Card and WireInt
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
