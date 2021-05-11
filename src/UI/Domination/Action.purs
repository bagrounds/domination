module Domination.UI.Domination.Action where

import Domination.Data.Play (Play)
import Domination.UI.Domination.ActiveState (ActiveState)

data Action
  = MakePlay Play
  | UndoRequest ActiveState
  | ToggleSupply
  | DoNothing

