module Domination.UI.ChoiceTrashUpTo
  ( component
  ) where

import Prelude

import Data.Array (filter)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Data.Choice (Choice(..))
import Domination.Data.Player (Player)
import Domination.UI.CardChooser as CardChooser
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
      (TrashUpTo { n, resolution: Nothing }) -> Just
        { title: "Trash up to " <> show n <> " cards"
        , buttonText: "Done trashing cards"
        }
      _ -> Nothing
    canToggle { selected, total } = selected || total < maxSelected
    maxSelected = case choice of
      TrashUpTo { n } -> n
      _ -> 0
    resolve resolution = case choice of
      TrashUpTo x -> TrashUpTo x { resolution = resolution }
      y -> y

