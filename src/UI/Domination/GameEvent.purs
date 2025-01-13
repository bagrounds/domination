--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a data type for game events in the Domination game.
--|
--| ### Key Concepts
--| * Game events and their types.
--| * The structure of a game event as a union of different variants.
--| * Associations between game states and active state.
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
