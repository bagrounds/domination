module Domination.UI.ChoiceDiscardDownTo
  ( component
  ) where

import Prelude

import Data.Array (filter, length)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Data.Card (Card)
import Domination.Data.Choice (Choice)
import Domination.Data.Choice as Choice
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.UI.Card as Card
import Domination.UI.Css as Css
import Halogen (Component)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)
import Halogen.Query.HalogenQ (HalogenQ)

type DiscardState = Array (Tuple Card Boolean)

data DiscardAction
  = ToggleDiscard Int Int
  | Done Int

component :: forall query input m. Player -> Component HTML query input Choice.Choice m
component player = H.mkComponent { initialState, render, eval }
  where
  initialState :: forall a. a -> Array (Tuple Card Boolean)
  initialState _ = (\x -> Tuple x false) <$> player.hand

  render :: forall a. DiscardState -> HTML a DiscardAction
  render xs = case Player.firstChoice player of
    Just (Choice.DiscardDownTo n Nothing) -> HH.h2_ $
      [ HH.text $ "Discard down to " <> show n <> " cards"
      , HH.button
        [ HP.class_ Css.resolveChoice, HE.onClick \_ -> Just $ Done n ]
        [ HH.text $ "Done discarding cards" ]
      ]
      <> renderCardToDiscard n `mapWithIndex` xs
    _ -> HH.h2_ [ HH.text "Something has gone terribly wrong!" ]

  eval :: forall a
    . HalogenQ query DiscardAction input a
    -> HalogenM DiscardState DiscardAction () Choice m a
  eval = H.mkEval H.defaultEval
    { handleAction = case _ of
      ToggleDiscard n i -> do
        xs <- H.get
        let nKept = length $ (not <<< snd) `filter` xs
        H.modify_
          $ mapWithIndex
            \j (Tuple c b) -> Tuple c
              $ if i == j && (b || nKept > n)
                then not b
                else b
      Done n -> (toResolved n <$> H.get) >>= H.raise
    }

  toResolved :: Int -> DiscardState -> Choice.Choice
  toResolved n xs = Choice.DiscardDownTo n choice
    where
      choice = Just
        $ map snd
        $ filter fst
        $ (\i (Tuple _ b) -> (Tuple b i)) `mapWithIndex` xs

  renderCardToDiscard :: forall a
    . Int -> Int -> Tuple Card Boolean -> HTML a DiscardAction
  renderCardToDiscard n cardIndex (Tuple card selected) =
    Card.render onClick extraClasses card
    where
      onClick _ = Just (ToggleDiscard n cardIndex)
      extraClasses =
        [ if selected
          then Css.toTrash
          else Css.toKeep
        ]

