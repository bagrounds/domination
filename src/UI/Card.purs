module Domination.UI.Card
  ( render
  ) where

import Prelude

import Data.Array (intercalate)
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Domination.Capability.Dom (class Dom, stopPropagation)
import Domination.Capability.Log (class Log)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.CardType as CardType
import Domination.UI.Css as Css
import Domination.UI.Icons as Icons
import Domination.UI.RenderText (renderText)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_, classes) as HP
import Web.UIEvent.MouseEvent (toEvent)

--render
--  :: forall w i
--  . (MouseEvent -> Maybe i)
--  -> Array H.ClassName
--  -> Card
--  -> DomSlot
--  -> HTML w i
render onClick extraClasses card slot =
  HH.button properties children where
  properties =
    [ HE.onClick onClick
    , HP.classes
      $ [ Css.card ]
      <> extraClasses <>
      ( if Card.hasType CardType.Treasure card
        then [ Css.treasureCard ]
        else []
      ) <>
      ( if Card.hasType CardType.Victory card
        then [ Css.victoryCard ]
        else []
      ) <>
      ( if Card.hasType CardType.Attack card
        then [ Css.attackCard ]
        else []
      ) <>
      ( if Card.hasType CardType.Reaction card
        then [ Css.reactionCard ]
        else []
      ) <>
      ( if Card.hasType CardType.Action card
        then [ Css.actionCard ]
        else []
      ) <>
      ( if Card.hasType CardType.Curse card
        then [ Css.curseCard ]
        else []
      )
    ]
  children =
    [ HH.ul_
      [ HH.slot
        (SProxy :: SProxy "description")
        slot
        (descriptionComponent card)
        card
        (const Nothing)
      , HH.li
        [ HP.classes [ Css.cardText, Css.cardCards ] ]
          if card.cards > zero
          then
            [ HH.text $ "+" <> show card.cards
            , Icons.cards
            ]
          else []
      , HH.li
        [ HP.classes [ Css.cardText, Css.cardActions ] ]
          if card.actions > zero
          then
            [ HH.text "+"
            , renderText card.actions
            ]
          else []
      , HH.li
        [ HP.classes [ Css.cardText, Css.cardBuys ] ]
          if card.buys > zero
          then
            [ HH.text "+"
            , renderText card.buys
            ]
          else []
      , HH.li
        [ HP.classes [ Css.cardText, Css.cardTreasure ] ]
          if card.treasure > zero
          then
            [ HH.text $ "+" <> show card.treasure
            , Icons.money
            ]
          else []
      , HH.li
        [ HP.classes [ Css.cardText, Css.cardVictoryPoints ] ]
          if card.victoryPoints /= zero
          then
            [ HH.text
              $ (if card.victoryPoints > zero then "+" else "")
              <> show card.victoryPoints
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

descriptionComponent
  :: forall query output m
  . Dom m
  => Log m
  => Card
  -> H.Component HTML query Card output m
descriptionComponent card' =
  H.mkComponent { initialState, render: render', eval }
  where
    initialState _ = { card: card', visible: false }
    render' { visible, card } =
      if not (isJust card.special || isJust card.reaction)
      then HH.li
        [ HP.classes [ Css.cardText, Css.cardName ] ]
        [ HH.text card.name ]
      else HH.li
        [ HP.classes [ Css.description ] ] $
        [ HH.button
          [ HP.classes [ Css.cardText, Css.cardName, Css.toggle ]
          , HP.onClick \event ->
            Just { card, event: Just event, reset: false }
          ]
          [ HH.text card.name ]
        ] <>
        if visible
        then
          [ HH.button
            [ HP.class_ Css.toolTip
            , HP.onClick \event ->
              Just { card, event: Just event, reset: false }
            ] $
            [ HH.text $ description card.special ]
            <> case card.reaction of
              Nothing -> []
              Just reaction ->
                [ HH.hr_
                , renderText reaction
                ]
          ]
        else []
    description special = intercalate ". " (_.description <$> special)
    eval = H.mkEval H.defaultEval
      { handleAction = \{ event, reset, card } -> do
        case event of
          Nothing -> pure unit
          Just e -> stopPropagation $ toEvent e
        if reset
          then H.put { card, visible: false }
          else
            H.modify_ \{ visible } -> { visible: not visible, card: card }
      , receive = \card -> Just { event: Nothing, reset: true, card }
      }


