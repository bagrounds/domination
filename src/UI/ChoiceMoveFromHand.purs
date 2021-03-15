module Domination.UI.ChoiceMoveFromTo
  ( component
  ) where

import Prelude

import Data.Array (length)
import Data.Lens.Prism (review)
import Data.Maybe (Maybe(..))
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log)
import Domination.Data.Choice (Choice(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Pile as Pile
import Domination.Data.Player (Player)
import Domination.Data.Wire.Int as Int
import Domination.UI.CardChooser as CardChooser
import Domination.UI.DomSlot (DomSlot)
import Domination.UI.RenderText (renderText)
import Halogen (Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH

component
  :: forall query input m
  . Dom m
  => Log m
  => Player
  -> Choice
  -> (Int -> DomSlot)
  -> Component HTML query input Choice m
component player choice baseSlotNumber =
  CardChooser.component
    { renderChoice
    , canToggle
    , resolve
    , player
    , choice
    , pile
    , baseSlotNumber
    }
  where
    renderChoice = case _ of
      x@(MoveFromTo { n, resolution: Nothing }) -> Just
        { title: renderText x
        , buttonText: HH.text "Done"
        }
      _ -> Nothing
    canToggle { selected, total } =
      selected || total < maxSelected
    maxSelected = case choice of
      MoveFromTo { n } ->
        case n of
          UpTo n -> review Int._toWire n
          Exactly n -> review Int._toWire n
          DownTo n -> length cards - (review Int._toWire n)
      _ -> 0
    resolve resolution = case choice of
      MoveFromTo x ->
        MoveFromTo x { resolution = resolution }
      y -> y
    pile = case choice of
      MoveFromTo { source } -> source
      _ -> Pile.Trash
    cards = case pile of
      -- TODO pass in full game state so we can look at the trash
      Pile.Trash -> []
      Pile.Hand -> player.hand
      Pile.Discard -> player.discard
      Pile.ToDiscard -> player.toDiscard
      Pile.Deck -> player.deck

