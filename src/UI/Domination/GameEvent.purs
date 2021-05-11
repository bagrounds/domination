module Domination.UI.Domination.GameEvent where

import Data.Maybe (Maybe)
import Domination.Data.Game (Game)
import Domination.Data.Play (Play)
import Domination.UI.Domination.ActiveState (ActiveState)

data GameEvent
  = NewState
    ActiveState
    (Maybe { play :: Play, playerIndex :: Int, state :: Game })
  | SaveGame ActiveState
  | Undo ActiveState

