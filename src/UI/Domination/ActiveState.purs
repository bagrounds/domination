--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines data types, lenses, and functions for managing game states of a Domination game.
--|
--| ### Key Concepts
--| * Lens (Data.Lens.Lens) for data manipulation
--| * Record types and props (Data.Lens.Record.prop)
--| * Data transformation using Setter (%~)
module Domination.UI.Domination.ActiveState where

import Prelude

import Data.Lens.Lens (Lens', Lens)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Maybe (Maybe(..))
import Domination.Data.AI (Bot)
import Domination.Data.AI.Strategy (Strategy)
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Domination.Data.Play (Play)
import Halogen (ForkId)
import Type.Proxy (Proxy(..))

type ActiveState =
  { i :: Int
  , playerCount :: Int
  , playerIndex :: Int
  , showSupply :: Boolean
  , state :: Game
  }

type BotConfig =
  { botStrategies :: Array Strategy
  , botDelay :: Int
  }

type PendingBotPlay =
  { bot :: Bot
  , play :: Play
  , i :: Int
  , state :: Game
  }

type ComponentState =
  { active :: ActiveState
  , botConfig :: BotConfig
  , pendingBotPlay :: Maybe PendingBotPlay
  , botTimer :: Maybe ForkId
  }

mkComponentState :: BotConfig -> ActiveState -> ComponentState
mkComponentState botConfig active =
  { active
  , botConfig
  , pendingBotPlay: Nothing
  , botTimer: Nothing
  }

isPendingFor :: ActiveState -> PendingBotPlay -> Boolean
isPendingFor { i, state } pending = pending.i == i && pending.state == state

_active
  :: forall a b r
  . Lens { active :: a | r } { active :: b | r } a b
_active = prop (Proxy :: Proxy "active")

_i
  :: forall a b r
  . Lens { i :: a | r } { i :: b | r } a b
_i = prop (Proxy :: Proxy "i")
_playerCount :: Lens' ActiveState Int
_playerCount = prop (Proxy :: Proxy "playerCount")
_playerIndex :: Lens' ActiveState Int
_playerIndex = prop (Proxy :: Proxy "playerIndex")
_showSupply
  :: forall a b r
  . Lens { showSupply :: a | r } { showSupply :: b | r } a b
_showSupply = prop (Proxy :: Proxy "showSupply")
_state
  :: forall a b r
  . Lens { state :: a | r } { state :: b | r } a b
_state = prop (Proxy :: Proxy "state")

upgrade :: ActiveState -> ActiveState
upgrade = _state %~ Game.upgrade
