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
import Type.Proxy (Proxy(..))
import Domination.Data.AI (Bot)
import Domination.Data.AI.Strategy (botName)
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Halogen (SubscriptionId)

type ActiveState =
  { i :: Int
  , playerCount :: Int
  , playerIndex :: Int
  , showSupply :: Boolean
  , state :: Game
  , bots :: Array Bot
  }

type ComponentState =
  { active :: ActiveState
  , pendingBotMessage :: Maybe String
  , botTimerSub :: Maybe SubscriptionId
  }

mkComponentState :: ActiveState -> ComponentState
mkComponentState active =
  { active
  , pendingBotMessage: Nothing
  , botTimerSub: Nothing
  }

_active
  :: forall a b r
  . Lens { active :: a | r } { active :: b | r } a b
_active = prop (Proxy :: Proxy "active")

_pendingBotMessage
  :: forall a b r
  . Lens { pendingBotMessage :: a | r } { pendingBotMessage :: b | r } a b
_pendingBotMessage = prop (Proxy :: Proxy "pendingBotMessage")

_botTimerSub
  :: forall a b r
  . Lens { botTimerSub :: a | r } { botTimerSub :: b | r } a b
_botTimerSub = prop (Proxy :: Proxy "botTimerSub")

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
_bots :: Lens' ActiveState (Array Bot)
_bots = prop (Proxy :: Proxy "bots")

upgrade :: ActiveState -> ActiveState
upgrade = _state %~ Game.upgrade
