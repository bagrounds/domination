--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Creates a UI component for making a choice in a card game, including rendering options and handling selection and resolution.
--|
--| ### Key Concepts
--| * `Dom` and `Log` classes
--| * `Game` data type
--| * `Choice` data type (specifically `MoveFromTo`)
module Domination.UI.ChoiceMoveFromTo where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log)
import Domination.Data.Choice (Choice(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Game (Game)
import Domination.Data.Game as Game
import Domination.Data.Pile as Pile
import Domination.Data.Player (Player)
import Domination.UI.CardChooser as CardChooser
import Domination.UI.DomSlot (DomSlot)
import Domination.UI.RenderText (renderText)
import Halogen (Component)
import Halogen.HTML as HH

component
  :: forall query input m
  . Dom m
  => Log m
  => Game
  -> Player
  -> Int
  -> Choice
  -> (Int -> DomSlot)
  -> Component query input Choice m
component state player playerIndex choice baseSlotNumber =
  CardChooser.component
    { renderChoice
    , canToggle
    , resolve
    , player
    , playerIndex
    , state
    , choice
    , pile
    , baseSlotNumber
    }

  where
    renderChoice = case _ of
      x@(MoveFromTo { resolution: Nothing }) -> Just
        { title: renderText x
        , buttonText: HH.text "Done"
        }
      _ -> Nothing

    canToggle { selected, total } = selected || total < maxSelected

    maxSelected = case choice of
      MoveFromTo { n: n' } -> case n' of
        UpTo n -> n
        Exactly n -> n
        DownTo n -> length cards - n
        Unlimited -> length cards
      _ -> 0

    resolve resolution = case choice of
      MoveFromTo x -> MoveFromTo x { resolution = resolution }
      y -> y

    pile = case choice of
      MoveFromTo { source } -> source
      _ -> Pile.Trash

    cards = Game.pileCards pile playerIndex state
