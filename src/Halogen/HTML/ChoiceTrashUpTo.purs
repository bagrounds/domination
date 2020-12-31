module Halogen.HTML.ChoiceTrashUpTo
  ( component
  ) where

import Prelude

import Data.Array
import Data.Maybe
import Data.Tuple
import Dominion
import Player as Player
import Choice as Choice
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
        [ HP.class_ cssClass.resolveChoice, HE.onClick \_ -> Just $ Done n ]
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
  (if isTreasure card then [ HP.class_ cssClass.treasureCard ] else [ HP.class_ cssClass.noTreasureCard ])
  [ HH.div (if isVictory card then [ HP.class_ cssClass.victoryCard ] else [ HP.class_ cssClass.noVictoryCard ])
    [ HH.div (if isAction card then [ HP.class_ cssClass.actionCard ] else [ HP.class_ cssClass.noActionCard ])
      [ HH.button
        [ HE.onClick \_ -> Just (ToggleTrash n cardIndex)
        , HP.classes
          [ cssClass.card
          , if selected
            then cssClass.toTrash
            else cssClass.toKeep
          ]
        ]
        [ HH.ul_
          [ HH.li
            [ HP.classes [ cssClass.cardText, cssClass.cardName ] ]
            [ HH.text $ " " <> card.name ]
          , HH.li
            [ HP.classes [ cssClass.cardText, cssClass.cardCards ] ]
            [ HH.text (if card.cards > 0 then " +" <> show card.cards <> " Card" else "") ]
          , HH.li
            [ HP.classes [ cssClass.cardText, cssClass.cardActions ] ]
            [ HH.text $ (if card.actions > 0 then " +" <> show card.actions <> " Action" else "") ]
          , HH.li
            [ HP.classes [ cssClass.cardText, cssClass.cardBuys ] ]
            [ HH.text (if card.buys > 0 then " +" <> show card.buys <> " Buy" else "") ]
          , HH.li
            [ HP.classes [ cssClass.cardText, cssClass.cardTreasure ] ]
            [ HH.text (if card.treasure > 0 then " +$" <> show card.treasure else "") ]
          , HH.li
            [ HP.classes [ cssClass.cardText, cssClass.cardVictoryPoints ] ]
            [ HH.text
              ( if card.victoryPoints > 0
                then " +" <> show card.victoryPoints <> " VP"
                else if card.victoryPoints < 0
                  then show card.victoryPoints
                  else ""
              )
            ]
          , HH.li
            [ HP.classes [ cssClass.cardText, cssClass.cardCost ] ]
            [ HH.text $ "Cost $" <> show card.cost ]
          ]
        ]
      ]
    ]
  ]

cssClass =
  { stats: H.ClassName "stats"
  , stat: H.ClassName "stat"
  , supply: H.ClassName "supply"
  , hand: H.ClassName "hand"
  , handInfo: H.ClassName "hand-info"
  , handInfoArea: H.ClassName "hand-info-area"
  , play: H.ClassName "play"
  , buying: H.ClassName "buying"
  , deckArea: H.ClassName "deck-area"
  , deck: H.ClassName "deck"
  , discard: H.ClassName "discard"
  , card: H.ClassName "card"
  , actionCard: H.ClassName "action-card"
  , noActionCard: H.ClassName "no-action-card"
  , treasureCard: H.ClassName "treasure-card"
  , noTreasureCard: H.ClassName "no-treasure-card"
  , victoryCard: H.ClassName "victory-card"
  , noVictoryCard: H.ClassName "no-victory-card"
  , cardName: H.ClassName "card-name"
  , cardCards: H.ClassName "card-cards"
  , cardActions: H.ClassName "card-actions"
  , cardBuys: H.ClassName "card-buys"
  , cardTreasure: H.ClassName "card-treasure"
  , cardVictoryPoints: H.ClassName "card-victory-points"
  , cardCost: H.ClassName "card-cost"
  , cardText: H.ClassName "card-text"
  , stack: H.ClassName "stack"
  , stackCard: H.ClassName "stack-card"
  , stackCount: H.ClassName "stack-count"
  , active: H.ClassName "active"
  , waiting: H.ClassName "waiting"
  , inactive: H.ClassName "inactive"
  , canBuy: H.ClassName "can-buy"
  , cantBuy: H.ClassName "cant-buy"
  , canPlay: H.ClassName "can-play"
  , cantPlay: H.ClassName "cant-play"
  , nextPhase: H.ClassName "next-phase"
  , resolveChoice: H.ClassName "resolve-choice"
  , toTrash: H.ClassName "to-trash"
  , toKeep: H.ClassName "to-keep"
  }

