module AppAction where

import AppState (AppState)
import Data.Lens.Lens (Lens')
import Domination.Data.Card (CardSpec)
import Domination.UI.Domination.GameEvent (GameEvent)
import Web.Event.Event (Event)

type CardSpecSelection = { cardSpec :: CardSpec, selected :: Boolean }

data AppAction
  = Initialize
  | StartNewGame
  | WritePlayerIndex Int
  | WritePlayerCount Int
  | LoadGameRequest
  | ToggleMenu
  | ToggleLongGame
  | RandomizeKingdom
  | ChooseKingdom (Array CardSpecSelection)
  | WriteUsername String
  | Write (Lens' AppState String) String
  | SendMessage
  | ReceiveRemoteMessage Event
  | ReceiveLocalMessage Event
  | HandleGameEvent GameEvent
  | DoNothing

