--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A PureScript module for a card choice component in a card game.
--|
--| ### Key Concepts
--| * `Dom` and `Log` classes are used for game-related functionality.
--| * The `Game` type represents the current state of the game, including cards and constraints.
--| * The `ComponentSpec` type defines the layout and behavior of the component.

module Domination.UI.ChooseCards where

import Prelude
import Prim hiding (Constraint)

import Data.Array (filter, length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log)
import Domination.Data.Card (Card)
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Filter (Filter)
import Domination.Data.Game (Game)
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Player (Player)
import Domination.Data.Stack (stacksToCards)
import Domination.UI.Card as Card
import Domination.UI.Css as Css
import Domination.UI.DomSlot (DomSlot)
import Halogen as H
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Array (Tuple Card Boolean)

data Action
  = Toggle Int
  | Done

type ComponentSpec =
  { player :: Player
  , pile :: Pile
  , baseSlotNumber :: Int -> DomSlot
  , state :: Game
  , constraint :: Constraint
  , filter :: Filter
  }

component
  :: forall query m
  . Dom m
  => Log m
  => ComponentSpec
  -> Component query Unit (Array Int) m
component
  { baseSlotNumber
  , state
  , player
  , pile
  , constraint
  } = H.mkComponent { initialState, render, eval }
  where
    initialState :: forall a. a -> Array (Tuple Card Boolean)
    initialState _ = (\x -> Tuple x false) <$> cards
    render xs = HH.div_ $
      [ HH.text "Choose cards"
      , HH.p_
        [ HH.button
          [ HP.class_ Css.resolveChoice
          , HE.onClick \_ -> Done
          ]
          [ HH.text "Done" ]
        ]
      ]
      <> renderCard `mapWithIndex` xs

    eval = H.mkEval H.defaultEval
      { handleAction = case _ of
        Toggle i -> do
          xs <- H.get
          let total = length $ snd `filter` xs
          H.modify_
            $ mapWithIndex
              \j (Tuple c selected) -> Tuple c
                $ if i == j
                  && canToggle { selected, total }
                  then not selected
                  else selected
        Done -> (resolution <$> H.get) >>= H.raise
      }
      where
        canToggle { selected, total } =
          selected || total < maxSelected

        maxSelected = case constraint of
          UpTo n -> n
          Exactly n -> n
          DownTo n -> length cards - n
          Unlimited -> length cards

        resolution :: State -> Array Int
        resolution xs = map snd
          $ filter fst
          $ (\i (Tuple _ b) -> (Tuple b i)) `mapWithIndex` xs

    renderCard cardIndex (Tuple card selected) =
      Card.render onClick extraClasses card (baseSlotNumber cardIndex)
      where
        onClick _ = Toggle cardIndex
        extraClasses =
          [ if selected
            then Css.toTrash
            else Css.toKeep
          ]

    cards = case pile of
      Pile.AtPlay -> player.atPlay
      Pile.Hand -> player.hand
      Pile.Discard -> player.discard
      Pile.Buying -> player.buying
      Pile.Discarding -> player.toDiscard
      Pile.Deck -> player.deck
      Pile.Trash -> state.trash
      Pile.Supply -> stacksToCards state.supply
