module Domination.Data.Wire.GameState where

import Prelude

import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Lens.Setter ((%~))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Domination.Data.Wire.Card as Card
import Domination.Data.GameState (GameState, _players, _result, _supply, _trash, _turn)
import Domination.Data.Phase (Phase)
import Domination.Data.Player (WirePlayer)
import Domination.Data.Player as Player
import Domination.Data.Wire.Result (WireResult)
import Domination.Data.Wire.Result as Result
import Domination.Data.Wire.Stack (WireStack)
import Domination.Data.Wire.Supply as Supply
import Domination.Data.Wire.Int (WireInt)
import Domination.Data.Wire.Int as Int
import Util ((<$>~))

type WireGameState = Tuple Phase
  (Tuple (Array WirePlayer)
  (Tuple (Array WireStack)
  (Tuple (Array WireInt)
  (Tuple WireInt
  (Tuple (Maybe WireResult) Boolean)))))

fromWire :: WireGameState -> GameState
fromWire = review _toWire

_toWire :: Iso' GameState WireGameState
_toWire = iso to from where
  to = (_turn %~ view Int._toWire)
    >>> (_players <$>~ view Player._toWire)
    >>> (_supply %~ view Supply._toWire)
    >>> (_trash <$>~ view Card._toWire)
    >>> (_result <$>~ view Result._toWire)
    >>> toTuple
  from = fromTuple
    >>> (_turn %~ review Int._toWire)
    >>> (_players <$>~ review Player._toWire)
    >>> (_supply %~ review Supply._toWire)
    >>> (_trash <$>~ (review Card._toWire))
    >>> (_result <$>~ review Result._toWire)
  toTuple { phase, players, result, supply, trash, turn, longGame } =
    Tuple phase $ Tuple players $ Tuple supply $ Tuple trash
      $ Tuple turn $ Tuple result longGame
  fromTuple
    (Tuple phase (Tuple players (Tuple supply (Tuple trash
    (Tuple turn (Tuple result longGame)))))) =
    { phase, players, result, supply, trash, turn, longGame }

