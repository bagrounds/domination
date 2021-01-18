module Domination.UI.ChoiceTrashExactly
  ( component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Domination.Data.Choice (Choice(..))
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
      x@(TrashExactly { n, resolution: Nothing }) -> Just
        { title: Choice.renderText' x
        , buttonText: "Done trashing cards"
        }
      _ -> Nothing
    canToggle { selected, total } = selected || total < maxSelected
    maxSelected = case choice of
      TrashExactly { n } -> n
      _ -> 0
    resolve resolution = case choice of
      TrashExactly x -> TrashExactly x { resolution = resolution }
      y -> y

