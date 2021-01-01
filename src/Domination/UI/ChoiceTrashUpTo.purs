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
import Domination.Data.Card as Card
import Domination.Data.Choice as Choice
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.UI.Css as Css
import Halogen (Component)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type TrashState = Array (Tuple Card Boolean)
data TrashAction = ToggleTrash Int Int | Done Int
component :: forall query o m. Player -> Component HTML query o Choice.Choice m
component player = H.mkComponent { initialState, render, eval }
  where
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
  eval = H.mkEval H.defaultEval
    { handleAction = case _ of
      ToggleTrash n i -> do
        xs <- H.get
        let nSelected = length $ snd `filter` xs
        H.modify_ $ mapWithIndex \j (Tuple c b) -> Tuple c (if i == j && (b || nSelected < n) then not b else b)
      Done n -> (toResolved n <$> H.get) >>= H.raise
    }
  toResolved :: Int -> TrashState -> Choice.Choice
  toResolved n xs = Choice.TrashUpTo n (Just $ map snd $ filter fst (mapWithIndex (\i (Tuple _ b) -> (Tuple b i)) xs))

renderCardToTrash :: forall a. Int -> Int -> Tuple Card Boolean -> HTML a TrashAction
renderCardToTrash n cardIndex (Tuple card selected) = HH.div
  (if Card.isTreasure card then [ HP.class_ Css.treasureCard ] else [ HP.class_ Css.noTreasureCard ])
  [ HH.div (if Card.isVictory card then [ HP.class_ Css.victoryCard ] else [ HP.class_ Css.noVictoryCard ])
    [ HH.div (if Card.isAction card then [ HP.class_ Css.actionCard ] else [ HP.class_ Css.noActionCard ])
      [ HH.button
        [ HE.onClick \_ -> Just (ToggleTrash n cardIndex)
        , HP.classes
          [ Css.card
          , if selected
            then Css.toTrash
            else Css.toKeep
          ]
        ]
        [ HH.ul_
          [ HH.li
            [ HP.classes [ Css.cardText, Css.cardName ] ]
            [ HH.text $ " " <> card.name ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardCards ] ]
            [ HH.text (if card.cards > 0 then " +" <> show card.cards <> " Card" else "") ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardActions ] ]
            [ HH.text $ (if card.actions > 0 then " +" <> show card.actions <> " Action" else "") ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardBuys ] ]
            [ HH.text (if card.buys > 0 then " +" <> show card.buys <> " Buy" else "") ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardTreasure ] ]
            [ HH.text (if card.treasure > 0 then " +$" <> show card.treasure else "") ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardVictoryPoints ] ]
            [ HH.text
              ( if card.victoryPoints > 0
                then " +" <> show card.victoryPoints <> " VP"
                else if card.victoryPoints < 0
                  then show card.victoryPoints
                  else ""
              )
            ]
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardCost ] ]
            [ HH.text $ "Cost $" <> show card.cost ]
          ]
        ]
      ]
    ]
  ]

