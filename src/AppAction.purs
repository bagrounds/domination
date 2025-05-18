--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a set of actions for an application's state changes.
--|
--| ### Key Concepts
--| * **Actions**: A set of discrete events that can be triggered in the application.
--| * **State Updates**: How the application's state changes in response to these actions.
--| * **Event Handling**: The process of responding to incoming events and messages.
module AppAction where

import AppState (AppState)
import Data.Lens.Lens (Lens')
import Domination.Data.Card (CardSpec)
import Domination.UI.Domination.GameEvent (GameEvent)
import Web.Event.Event (Event)

type CardSpecSelection = { cardSpec :: CardSpec, selected :: Boolean }

data AppAction
  = Initialize
  | Finalize
  | HeartbeatTick
  | StartNewGame
  | WritePlayerIndex Int
  | WritePlayerCount Int
  | LoadGameRequest
  | ToggleMenu
  | ToggleLongGame
  | RandomizeKingdom
  | ChooseKingdom (Array CardSpecSelection)
  | WriteServerUrl String
  | WriteUsername String
  | Write (Lens' AppState String) String
  | SendMessage
  | ReceiveRemoteMessage Event
  | HandleGameEvent GameEvent
  | DoNothing
