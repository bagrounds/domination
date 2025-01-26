--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Converts a stack data structure into a tuple of wire integers.
--|
--| ### Key Concepts
--| * **Lens**: A powerful functional programming technique for manipulating data structures.
--| * **Isomorphism**: A bijective mapping between two types that preserves their structure.
--| * **Type Transformation**: The process of creating a new type from an existing one using functions and higher-order types.
module Domination.Data.Wire.Stack where

import Prelude

import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Lens.Setter ((%~))
import Data.Tuple (Tuple(..))
import Domination.Data.Wire.Card as Card
import Domination.Data.Stack (Stack, _card, _count)
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int

type WireStack = Tuple WireInt WireInt

_toWire :: Iso' Stack WireStack
_toWire = iso to from where
  to = (_card %~ view Card._toWire)
    >>> (_count %~ view Int._toWire)
    >>> toTuple
  from = fromTuple
    >>> (_card %~ review Card._toWire)
    >>> (_count %~ review Int._toWire)
  toTuple { card, count } = Tuple card count
  fromTuple (Tuple card count) = { card, count }
