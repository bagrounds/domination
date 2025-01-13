--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines actions for a domination game.
--|
--| ### Key Concepts
--| * Data type for actions in the module.
--| * Types of actions that can be performed.
--| * No action results in no change.
module Domination.UI.Domination.Action where

import Domination.Data.Play (Play)
import Domination.UI.Domination.ActiveState (ActiveState)

data Action
  = MakePlay Play
  | UndoRequest ActiveState
  | ToggleSupply
  | DoNothing
