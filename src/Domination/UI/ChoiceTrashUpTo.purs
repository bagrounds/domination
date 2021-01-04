module Domination.UI.ChoiceTrashUpTo
  ( component
  ) where

import Prelude

import Data.Array (filter)
import Data.Foldable (length)
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
import Halogen.Query.HalogenM (HalogenM(..))
import Halogen.Query.HalogenQ (HalogenQ)

type TrashState = Array (Tuple Card Boolean)

data TrashAction
  = ToggleTrash Int Int
  | Done Int

component :: forall query input m. Player -> Component HTML query input Choice.Choice m
component player = H.mkComponent { initialState, render, eval }
  where
  initialState :: forall a. a -> Array (Tuple Card Boolean)
  initialState _ = (\x -> Tuple x false) <$> player.hand

  render :: forall a. TrashState -> HTML a TrashAction
  render xs = case Player.firstChoice player of
    Just (Choice.TrashUpTo n Nothing) -> HH.h2_ $
      [ HH.text $ "Trash up to " <> show n <> " cards"
      , HH.button
        [ HP.class_ Css.resolveChoice, HE.onClick \_ -> Just $ Done n ]
        [ HH.text $ "Done trashing cards" ]
      ]
      <> renderCardToTrash n `mapWithIndex` xs
    _ -> HH.h2_ [ HH.text "Something has gone terribly wrong!" ]

  eval :: forall a
    . HalogenQ query TrashAction input a
    -> HalogenM TrashState TrashAction () Choice m a
  eval = H.mkEval H.defaultEval
    { handleAction = case _ of
      ToggleTrash n i -> do
        xs <- H.get
        let nSelected = length $ snd `filter` xs
        H.modify_
          $ mapWithIndex
            \j (Tuple c b) -> Tuple c
              $ if i == j && (b || nSelected < n)
                then not b
                else b
      Done n -> (toResolved n <$> H.get) >>= H.raise
    }

  toResolved :: Int -> TrashState -> Choice.Choice
  toResolved n xs = Choice.TrashUpTo n choice
    where
      choice = Just
        $ map snd
        $ filter fst
        $ (\i (Tuple _ b) -> (Tuple b i)) `mapWithIndex` xs

  renderCardToTrash :: forall a
    . Int -> Int -> Tuple Card Boolean -> HTML a TrashAction
  renderCardToTrash n cardIndex (Tuple card selected) =
    Card.render onClick extraClasses card
    where
      onClick _ = Just (ToggleTrash n cardIndex)
      extraClasses =
        [ if selected
          then Css.toTrash
          else Css.toKeep
        ]

