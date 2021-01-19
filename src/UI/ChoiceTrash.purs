module Domination.UI.ChoiceTrash
  ( component
  ) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Domination.Data.Choice (Choice(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Player (Player)
import Domination.UI.CardChooser as CardChooser
import Domination.UI.Choice as Choice
import Halogen (Component)
import Halogen.HTML (HTML)

component
  :: forall query input m
  . Player
  -> Choice
  -> Component HTML query input Choice m
component player choice =
  CardChooser.component
    { renderChoice
    , canToggle
    , resolve
    , player
    , choice
    }
  where
    renderChoice = case _ of
      x@(Trash { n, resolution: Nothing }) -> Just
        { title: Choice.renderText' x
        , buttonText: "Done trashing cards"
        }
      _ -> Nothing
    canToggle { selected, total } = selected || total < maxSelected
    maxSelected = case choice of
      Trash { n } ->
        case n of
          UpTo n -> n
          Exactly n -> n
          DownTo n -> length player.hand - n
      _ -> 0
    resolve resolution = case choice of
      Trash x -> Trash x { resolution = resolution }
      y -> y

