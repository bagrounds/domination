module Domination.UI.Domination.GameEvent where

import Data.Maybe (Maybe)
import Domination.Data.GameState (GameState)
import Domination.Data.Play (Play)
import Domination.UI.Domination.ActiveState (ActiveState)

data GameEvent
  = NewState
    ActiveState
    (Maybe { play :: Play, playerIndex :: Int, state :: GameState })
  | SaveGame ActiveState
  | Undo ActiveState

