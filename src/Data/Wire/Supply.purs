--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Definition of an ISO between Supply data type and a WireSupply array of WireStacks.
--|
--| ### Key Concepts
--| * **Lenses**: A way to abstractly manipulate data by creating lenses that traverse the structure of a value.
--| * **Iso (Isomorphism)**: A type of lens that establishes an equivalence between two types.
--| * **Type conversion**: The process of converting one type into another, often using lenses.
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
