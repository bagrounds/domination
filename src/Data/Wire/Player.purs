module Domination.Data.Wire.Player where

import Prelude

import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Data.Actions (Actions)
import Domination.Data.Buys (Buys)
import Domination.Data.Player (Player, _atPlay, _bonuses, _buying, _choices, _deck, _discard, _hand, _pendingReactions, _toDiscard)
import Domination.Data.Wire.Bonus (WireBonus)
import Domination.Data.Wire.Bonus (_toWire) as Bonus
import Domination.Data.Wire.Card (_toWire) as Card
import Domination.Data.Wire.Choice (WireChoice)
import Domination.Data.Wire.Choice (_toWire) as Choice
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Reaction (WireReaction)
import Domination.Data.Wire.Reaction (_toWire) as Reaction
import Util ((<$>~))

type WirePlayer =
  (Tuple Actions
  (Tuple (Array WireInt) -- atPlay
  (Tuple (Array WireBonus) -- bonuses
  (Tuple (Array WireInt) -- buying
  (Tuple Buys
  (Tuple (Array WireChoice) -- choices
  (Tuple (Array WireInt) -- deck
  (Tuple (Array WireInt) -- discard
  (Tuple (Array WireInt) -- hand
  (Tuple (Array (Tuple WireReaction String)) -- pendingReactions
  (Array WireInt)))))))))))

_toWire :: Iso' Player WirePlayer
_toWire = iso to from where
  to = (_choices <$>~ view Choice._toWire)
    >>> (_bonuses <$>~ view Bonus._toWire)
    >>> (_deck <$>~ view Card._toWire)
    >>> (_hand <$>~ view Card._toWire)
    >>> (_discard <$>~ view Card._toWire)
    >>> (_toDiscard <$>~ view Card._toWire)
    >>> (_atPlay <$>~ view Card._toWire)
    >>> (_buying <$>~ view Card._toWire)
    >>> (_pendingReactions <$>~ \(Tuple r s) -> Tuple (view Reaction._toWire r) s)
    >>> toTuple
  from = fromTuple
    >>> (_choices <$>~ review Choice._toWire)
    >>> (_bonuses <$>~ review Bonus._toWire)
    >>> (_deck <$>~ review Card._toWire)
    >>> (_hand <$>~ review Card._toWire)
    >>> (_discard <$>~ review Card._toWire)
    >>> (_toDiscard <$>~ review Card._toWire)
    >>> (_atPlay <$>~ review Card._toWire)
    >>> (_buying <$>~ review Card._toWire)
    >>> (_pendingReactions <$>~ \(Tuple r s) -> Tuple (review Reaction._toWire r) s)
  toTuple
    { actions
    , atPlay
    , bonuses
    , buying
    , buys
    , choices
    , deck
    , discard
    , hand
    , pendingReactions
    , toDiscard
    } = Tuple actions
      $ Tuple atPlay
      $ Tuple bonuses
      $ Tuple buying
      $ Tuple buys
      $ Tuple choices
      $ Tuple deck
      $ Tuple discard
      $ Tuple hand
      $ Tuple pendingReactions toDiscard
  fromTuple
    (Tuple actions
    (Tuple atPlay
    (Tuple bonuses
    (Tuple buying
    (Tuple buys
    (Tuple choices
    (Tuple deck
    (Tuple discard
    (Tuple hand
    (Tuple pendingReactions toDiscard)))))))))) =
    { actions
    , atPlay
    , bonuses
    , buying
    , buys
    , choices
    , deck
    , discard
    , hand
    , pendingReactions
    , toDiscard
    }
