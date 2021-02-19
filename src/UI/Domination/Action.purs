module Domination.UI.Domination.Action where

import Domination.Data.Card (Card)
import Domination.Data.Play (Play)
import Domination.UI.Domination.ActiveState (ActiveState)

data Action
  = WritePlayerIndex Int
  | WritePlayerCount Int
  | StartNewGame
  | MakePlay Play
  | LoadGameRequest
  | UndoRequest ActiveState
  | ToggleSupply
  | ToggleMenu
  | ChooseKingdom (Array { card :: Card, selected :: Boolean })
  | Initialize

