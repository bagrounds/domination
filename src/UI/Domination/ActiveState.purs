--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines lenses and an upgrade function for managing game states in Domination UI.
--|
--| ### Key Concepts
--| * `Lens` and `prop` types for accessing and modifying record fields
--| * `ActiveState` record type with its constituent field definitions
--| * `_state Lens` upgrade function to modify the `Game.state` field of an `ActiveState` record.
module Domination.UI.Domination.ActiveState where

import Data.Lens.Lens (Lens', Lens)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Symbol (SProxy(..))
import Domination.Data.Game (Game)
import Domination.Data.Game as Game

type ActiveState =
  { i :: Int
  , playerCount :: Int
  , playerIndex :: Int
  , showSupply :: Boolean
  , state :: Game
  }

_i
  :: forall a b r
  . Lens { i :: a | r } { i :: b | r } a b
_i = prop (SProxy :: SProxy "i")
_playerCount :: Lens' ActiveState Int
_playerCount = prop (SProxy :: SProxy "playerCount")
_playerIndex :: Lens' ActiveState Int
_playerIndex = prop (SProxy :: SProxy "playerIndex")
_showSupply
  :: forall a b r
  . Lens { showSupply :: a | r } { showSupply :: b | r } a b
_showSupply = prop (SProxy :: SProxy "showSupply")
_state
  :: forall a b r
  . Lens { state :: a | r } { state :: b | r } a b
_state = prop (SProxy :: SProxy "state")

upgrade :: ActiveState -> ActiveState
upgrade = _state %~ Game.upgrade
