--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Definition for mapping between a supply and an array of wire stacks.
--|
--| ### Key Concepts
--| * Iso and Prism lenses
--| * Data types: `Supply`, `WireSupply`, `WireStack`
--| * `_toWire` function
module Domination.Data.Wire.Supply where

import Prelude

import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Domination.Data.Supply (Supply)
import Domination.Data.Wire.Stack (WireStack)
import Domination.Data.Wire.Stack (_toWire) as Stack

type WireSupply = Array WireStack

_toWire :: Iso' Supply WireSupply
_toWire = iso to from where
  to = map $ view Stack._toWire
  from = map $ review Stack._toWire
