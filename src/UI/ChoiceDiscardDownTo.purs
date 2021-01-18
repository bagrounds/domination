module Domination.UI.ChoiceDiscardDownTo where

import Prelude

import Data.Array (filter, length)
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
      (DiscardDownTo { n, resolution: Nothing }) -> Just
        { title: "Discard down to " <> show n <> " cards"
        , buttonText: "Done discarding"
        }
      _ -> Nothing
    canToggle { selected, total } = selected || total < maxSelected
    maxSelected = case choice of
      DiscardDownTo { n } ->
        length player.hand - n
      _ -> 0
    resolve resolution = case choice of
      DiscardDownTo x -> DiscardDownTo x { resolution = resolution }
      y -> y

