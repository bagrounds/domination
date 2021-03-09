module AppAction where

import AppState (AppState)
import Data.Lens.Lens (Lens')
import Domination.Data.Card (Card)
import Domination.UI.Domination.GameEvent (GameEvent)
import Web.Event.Event (Event)

data AppAction
  = Initialize
  | StartNewGame
  | WritePlayerIndex Int
  | WritePlayerCount Int
  | LoadGameRequest
  | ToggleMenu
  | ToggleLongGame
  | ChooseKingdom (Array { card :: Card, selected :: Boolean })
  | WriteUsername String
  | Write (Lens' AppState String) String
  | SendMessage
  | ReceiveRemoteMessage Event
  | ReceiveLocalMessage Event
  | HandleGameEvent GameEvent

