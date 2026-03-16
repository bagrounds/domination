--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a set of possible actions related to playing a game.
--|
--| ### Key Concepts
--| * Domain-specific data (Play and ActiveState)
--| * Action types
--| * State transition management
module Domination.UI.Domination.Action where

import Domination.Data.Play (Play)
import Domination.UI.Domination.ActiveState (ActiveState)

data Action
  = MakePlay Play
  | UndoRequest ActiveState
  | ToggleSupply
  | ExecutePendingBotPlay
  | DoNothing
