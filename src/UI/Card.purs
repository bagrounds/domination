module Domination.UI.Card
  ( render
  ) where

import Prelude

import Data.Array (intercalate, null)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Domination.Capability.Dom (class Dom, stopPropagation)
import Domination.Capability.Log (class Log, log)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.CardType as CardType
import Domination.UI.Css as Css
import Domination.UI.Icons as Icons
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events as HP
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

--render
--  :: forall w i
--  . (MouseEvent -> Maybe i)
--  -> Array H.ClassName
--  -> Card
--  -> HTML w i
render onClick extraClasses card slotNumber = HH.div
  ( if Card.isTreasure card
    then [ HP.class_ Css.treasureCard ]
    else [ HP.class_ Css.noTreasureCard ]
  )
  [ HH.div (if Card.isVictory card then [ HP.class_ Css.victoryCard ] else [ HP.class_ Css.noVictoryCard ])
    [ HH.div
      ( if Card.hasType CardType.Attack card
        then [ HP.class_ Css.attackCard ]
        else if Card.hasType CardType.Reaction card
        then [ HP.class_ Css.reactionCard ]
        else if Card.hasType CardType.Action card
        then [ HP.class_ Css.actionCard ]
        else if Card.hasType CardType.Curse card
        then [ HP.class_ Css.curseCard ]
        else [ HP.class_ Css.noActionCard ]
      )
      [ HH.button
        [ HE.onClick onClick
        , HP.classes $ [ Css.card ] <> extraClasses
        ]
        [ HH.ul_
          [ HH.slot
            (SProxy :: SProxy "description")
            slotNumber
            (descriptionComponent slotNumber card)
            card
            (const Nothing)
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardCards ] ]
              if card.cards > 0
              then
                [ HH.text $ show card.cards
                , Icons.cards
                ]
              else []
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardActions ] ]
              if card.actions > 0
              then
                [ HH.text $ show card.actions
                , Icons.actions
                ]
              else []
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardBuys ] ]
              if card.buys > 0
              then
                [ HH.text $ show card.buys
                , Icons.buys
                ]
              else []
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardTreasure ] ]
              if card.treasure > 0
              then
                [ HH.text $ show card.treasure
                , Icons.money
                ]
              else []
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardVictoryPoints ] ]
              if card.victoryPoints /= 0
              then
                [ HH.text $ show card.victoryPoints
                , Icons.points
                ]
              else []
          , HH.li
            [ HP.classes [ Css.cardText, Css.cardCost ] ]
            [ HH.text $ show card.cost
            , Icons.money
            ]
          ]
        ]
      ]
    ]
  ]

descriptionComponent
  :: forall query output m
  . Dom m
  => Log m
  => Int
  -> Card
  -> H.Component HTML query Card output m
descriptionComponent slotNumber card =
  H.mkComponent { initialState, render, eval }
  where
    initialState _ = { card, visible: false }
    render { visible, card } =
      case card.special of
      Nothing -> HH.li
        [ HP.classes [ Css.cardText, Css.cardName ] ]
        [ HH.text card.name ]
      Just _ -> HH.li
        [ HP.classes [ Css.description ] ] $
        [ HH.button
          [ HP.classes [ Css.cardText, Css.cardName, Css.toggle ]
          , HP.onClick \event -> Just  { card, event: Just event, reset: false }
          ]
          [ HH.text card.name ]
        ] <>
        if visible
        then
          [ HH.button
            [ HP.class_ Css.toolTip
            , HP.onClick \event -> Just { card, event: Just event, reset: false }
            ]
            [ HH.text $ description card.special ]
          ]
        else []
    description special = intercalate ". " (_.description <$> special)
    eval = H.mkEval H.defaultEval
      { handleAction = \{ event, reset, card } -> do
        case event of
          Nothing -> pure unit
          Just e -> stopPropagation $ toEvent e
        if reset
          then do
            log $ "(" <> card.name <> ") " <> "reset slotNumber: "
              <> show slotNumber
            H.put { card, visible: false }
          else do
            log $ "(" <> card.name <> ") " <> "toggle slotNumber: "
              <> show slotNumber
            H.modify_ \{ visible } -> { visible: not visible, card: card }
      , receive = \card -> Just { event: Nothing, reset: true, card }
      }


