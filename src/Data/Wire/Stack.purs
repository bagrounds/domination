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

