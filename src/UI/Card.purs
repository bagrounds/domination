--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Creates a reusable UI component for displaying cards with various details.
--|
--| ### Key Concepts
--| * **Component-based UI rendering**: The module uses Halogen's component-based approach to render HTML components.
--| * **Card data structure and manipulation**: The module deals with a Card data structure, which contains various properties (e.g., cards, actions, treasure) and methods for manipulating them.
--| * **Event handling and state management**: The module handles events (e.g., clicks) and manages the state of the component using Halogen's eval function.

module Domination.UI.Card
  ( render
  ) where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Foldable (sequence_)
import Data.Int (round)
import Data.Maybe (Maybe(..), isJust, fromMaybe, maybe)
import Domination.Capability.Dom (class Dom, boundingClientRect, stopPropagation, viewportSize)
import Domination.Capability.Log (class Log)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.CardType as CardType
import Domination.UI.Css as Css
import Domination.UI.DomSlot (DomSlot)
import Domination.UI.Icons as Icons
import Domination.UI.Placement (Position, placeTooltip, viewportMargin)
import Domination.UI.RenderText (renderText)
import Halogen as H
import Halogen.Component (ComponentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_, classes, ref, style) as HP
import Type.Proxy (Proxy(..))
import Web.DOM.Element (Element)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

render
  :: forall a b r m i
  . Dom m
  => Log m
  => (MouseEvent -> i)
  -> Array H.ClassName
  -> Card
  -> DomSlot
  -> HTML (ComponentSlot (description :: Slot a b DomSlot | r) m i) i
render onClick extraClasses card slot =
  HH.button properties children where
  properties =
    [ HE.onClick onClick
    , HH.attr (H.AttrName "aria-label") card.name
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
      [ HH.slot_
        (Proxy :: Proxy "description")
        slot
        (descriptionComponent card)
        card
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
            , renderText card.victoryPoints
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
  -> H.Component query Card output m
descriptionComponent card' =
  H.mkComponent { initialState, render: render', eval }
  where
    initialState _ = { card: card', visible: false, position: Nothing }
    render' { visible, card, position } =
      if not (isJust card.special || isJust card.reaction)
      then HH.li
        [ HP.classes [ Css.cardText, Css.cardName ] ]
        [ HH.text card.name ]
      else HH.li
        [ HP.classes [ Css.description ] ] $
        [ HH.button
          [ HP.classes [ Css.cardText, Css.cardName, Css.toggle ]
          , HP.ref tooltipAnchor
          , HP.onClick \event ->
            { card, event: Just event, reset: false }
          ]
          [ HH.text card.name ]
        ] <>
        if visible
        then
          [ HH.button
            [ HP.class_ Css.toolTip
            , HP.ref tooltip
            , HP.style $ maybe "" positionStyle position
            , HP.onClick \event ->
              { card, event: Just event, reset: false }
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
    description special = fromMaybe "" (_.description <$> special)
    eval = H.mkEval H.defaultEval
      { handleAction = \{ event, reset, card } -> do
        case event of
          Nothing -> pure unit
          Just e -> stopPropagation $ toEvent e
        if reset
          then H.put { card, visible: false, position: Nothing }
          else do
            H.modify_ \{ visible } ->
              { visible: not visible, card: card, position: Nothing }
            { visible } <- H.get
            when visible do
              anchorElement <- H.getRef tooltipAnchor
              tooltipElement <- H.getRef tooltip
              sequence_ $ positionTooltip <$> anchorElement <*> tooltipElement
      , receive = \card -> Just { event: Nothing, reset: true, card }
      }

tooltipAnchor :: H.RefLabel
tooltipAnchor = H.RefLabel "tooltip-anchor"

tooltip :: H.RefLabel
tooltip = H.RefLabel "tooltip"

positionTooltip
  :: forall state m
  . Dom m
  => MonadState { position :: Maybe Position | state } m
  => Element
  -> Element
  -> m Unit
positionTooltip anchorElement tooltipElement = do
  viewport <- viewportSize
  anchor <- boundingClientRect anchorElement
  { width, height } <- boundingClientRect tooltipElement
  H.modify_ _
    { position = Just $ placeTooltip
      { viewport
      , anchor
      , tooltip: { width, height }
      , margin: viewportMargin
      }
    }

positionStyle :: Position -> String
positionStyle { left, top } =
  "left: " <> pixels left <> "; top: " <> pixels top <> ";"
  where
  pixels n = show (round n) <> "px"
