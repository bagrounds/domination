module Domination.UI.ChooseCards where

import Prelude

import Data.Array (filter)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log)
import Domination.Data.Card (Card)
import Domination.Data.GameState (GameState)
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Player (Player)
import Domination.UI.Card as Card
import Domination.UI.Css as Css
import Domination.UI.DomSlot (DomSlot)
import Halogen as H
import Halogen.Component (Component)
import Halogen.HTML (HTML)
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
  , state :: GameState
  }

component
  :: forall query m
  . Dom m
  => Log m
  => ComponentSpec
  -> Component HTML query Unit (Array Int) m
component { baseSlotNumber, state, player, pile } =
  H.mkComponent { initialState, render, eval }
    where
    initialState :: forall a. a -> Array (Tuple Card Boolean)
    initialState _ = (\x -> Tuple x false) <$> cards
      where
        cards = case pile of
          Pile.Hand -> player.hand
          Pile.Discard -> player.discard
          Pile.ToDiscard -> player.toDiscard
          Pile.Deck -> player.deck
          Pile.Trash -> state.trash

    render xs = HH.div_ $
      [ HH.text "Choose cards"
      , HH.p_
        [ HH.button
          [ HP.class_ Css.resolveChoice
          , HE.onClick \_ -> Just $ Done
          ]
          [ HH.text "Done" ]
        ]
      ]
      <> renderCard `mapWithIndex` xs

    eval = H.mkEval H.defaultEval
      { handleAction = case _ of
        Toggle i -> do
          xs <- H.get
          H.modify_
            $ mapWithIndex
              \j (Tuple c selected) -> Tuple c
                $ if i == j
                  then not selected
                  else selected
        Done -> (resolution <$> H.get) >>= H.raise
      }
      where
        resolution :: State -> Array Int
        resolution xs = map snd
          $ filter fst
          $ (\i (Tuple _ b) -> (Tuple b i)) `mapWithIndex` xs

    renderCard cardIndex (Tuple card selected) =
      Card.render onClick extraClasses card (baseSlotNumber cardIndex)
      where
        onClick _ = Just (Toggle cardIndex)
        extraClasses =
          [ if selected
            then Css.toTrash
            else Css.toKeep
          ]

