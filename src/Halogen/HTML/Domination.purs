module Halogen.HTML.Domination
  ( component
  , GameUpdate(..)
  ) where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Control.Monad.Loops (untilJust)
import Data.Array (intercalate, mapWithIndex, (:), filter, (!!))
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple
import Dominion (Player(..), Card, GameState, Phase(..), Player, Stack, cash, hasActionCardsInHand, hasActions, isAction, isTreasure, isVictory, newGame, nextPhase, play, purchase, score, setup, choiceTurn, resolveChoice)
import Player as Player
import Choice as Choice
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen (Component)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.ChoiceTrashUpTo as Trash
import Halogen.Query.HalogenM (HalogenM)
import Web.UIEvent.MouseEvent (MouseEvent)

data GameUpdate = UpdateState GameState | NewGame Int

derive instance genericGameUpdate :: Generic GameUpdate _
instance showGameUpdate :: Show GameUpdate where
  show = genericShow

data GameAction = Receive GameUpdate
  | NextPhase Int
  | Play Int Int
  | Purchase Int Player Stack
  | ResolveChoice Choice.Choice

derive instance genericGameAction :: Generic GameAction _
instance showGameAction :: Show GameAction where
  show = genericShow

component :: forall query m. MonadAff m => Int -> Int -> Component HTML query GameUpdate GameState m
component playerCount playerIndex = H.mkComponent { initialState, render, eval }
  where
  initialState _ = newGame playerCount
  render = renderPlayerN playerIndex
  eval = H.mkEval H.defaultEval
    { initialize = Just $ Receive $ NewGame playerCount
    , handleAction = handleAction playerIndex
    , receive = Just <<< Receive
    }

--renderPlayerN :: forall b. Int -> GameState -> HTML b GameAction
renderPlayerN playerIndex state = HH.div_
  [ HH.h1 [] [ HH.text "Domination" ]
  , HH.div_ $ renderPlayers playerIndex state
  ]

renderSupply :: forall a. Int -> Player -> GameState -> Array (HTML a GameAction)
renderSupply playerIndex player state =
  renderStack playerIndex player <$> state.supply

renderDeck :: forall a. Player -> HTML a GameAction
renderDeck player = HH.button
  [ HE.onClick (const Nothing), HP.class_ cssClass.deck ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Discard" ]
    , HH.li [] [ HH.text $ show ((length player.discard) :: Int) ]
    ]
  ]

renderDiscard :: forall a. Player -> HTML a GameAction
renderDiscard player = HH.button
  [ HE.onClick (const Nothing), HP.class_ cssClass.discard ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Deck" ]
    , HH.li [] [ HH.text $ show ((length player.deck) :: Int) ]
    ]
  ]

renderCard :: forall a. (MouseEvent -> Maybe GameAction) -> Player -> Card -> HTML a GameAction
renderCard onClick player card = HH.div
  (if isTreasure card then [ HP.class_ cssClass.treasureCard ] else [ HP.class_ cssClass.noTreasureCard ])
  [ HH.div (if isVictory card then [ HP.class_ cssClass.victoryCard ] else [ HP.class_ cssClass.noVictoryCard ])
    [ HH.div (if isAction card then [ HP.class_ cssClass.actionCard ] else [ HP.class_ cssClass.noActionCard ])
      [ HH.button
        [ HE.onClick onClick
        , HP.classes
          [ cssClass.card
          , if player.actions > 0 && isAction card
            then cssClass.canPlay
            else cssClass.cantPlay
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


renderStack :: forall a. Int -> Player -> Stack -> HTML a GameAction
renderStack playerIndex player stack =
  HH.li [ HP.class_ cssClass.stack ]
    [ HH.ul_
      [ HH.li
        [ HP.class_ cssClass.stackCount ]
        [ HH.text $ show stack.count ]
      , HH.li
        [ HP.classes
          [ cssClass.stackCard
          , if player.buys > 0 && cash player >= stack.card.cost && stack.count > 0
            then cssClass.canBuy
            else cssClass.cantBuy
          ]
        ]
        [ renderCard (\_ -> Just $ Purchase playerIndex player stack) player stack.card ]
      ]
    ]

--renderPlayers :: forall a. Int -> GameState -> Array (HTML a GameAction)
renderPlayers i state = case state.players !! i of
  Nothing -> []
  Just player -> [ renderPlayer state i player ]

playerStats :: forall a. GameState -> Int -> Player -> (HTML a GameAction)
playerStats state playerIndex player = HH.li
  [ HP.class_ cssClass.stat ]
  [ HH.text $ "Player " <> show playerIndex
    <> ": Actions: " <> show player.actions
    <> "/" <> show (length (isAction `filter` player.hand) :: Int)
    <> " | $" <> show (cash player)
    <> " | Buys: " <> show player.buys
    <> " | Play: " <> (intercalate ", " (_.name <$> player.atPlay))
    <> " | Buying: " <> (intercalate ", " (_.name <$> player.buying))
    <> " | VP: " <> show (score player)
    <> (if state.turn == playerIndex then " | " <> show state.phase else "")
  ]

--renderPlayer :: forall a. GameState -> Int -> Player -> HTML a GameAction
renderPlayer state playerIndex player =
  if Player.hasChoices player && playerIndex == choiceTurn state
  then HH.div_ [ (HH.slot (SProxy :: SProxy "TrashUpTo") 0 (Trash.component player) unit (Just <<< ResolveChoice)) ]
  else
  case state.players !! state.turn of
    Nothing -> HH.h1_ [ HH.text "Something has gone terribly wrong!" ]
    Just currentPlayer -> HH.div
      ( if state.turn /= playerIndex
        then [ HP.class_ cssClass.waiting ]
        else []
      )
      [ HH.h2 [] [ HH.text $ "Player " <> show playerIndex ]
      , HH.ul
          [ HP.classes $
            [ cssClass.supply
            , if state.turn == playerIndex && state.phase == BuyPhase
              then cssClass.active
              else cssClass.inactive
            ] <> if player.buys < 1
              then [ cssClass.waiting ]
              else []
          ]
          $ HH.li_ [(HH.h3 [] [ HH.text $ "Supply" ])]
            : HH.ul_
              [ HH.li
                [ HP.class_ cssClass.handInfo ]
                [ HH.text $ "$" <> (show ((cash player) :: Int)) ]
              , HH.li
                [ HP.class_ cssClass.handInfo ]
                [ HH.text $ (show $ player.buys) <> " Buys" ]
              ]
            : [ HH.ul_ (renderSupply playerIndex player state) ]
      , HH.button
        [ HP.class_ (cssClass.nextPhase), HE.onClick \_ -> Just $ NextPhase playerIndex ]
        [ HH.text if state.turn == playerIndex
          then case state.phase of
            ActionPhase -> "Complete Action Phase"
            BuyPhase -> "Complete Buy Phase"
            CleanupPhase -> "Complete Turn"
          else "Waiting for Player " <> show state.turn
        ]
      , HH.ul
        [ HP.class_ cssClass.stats ]
        (playerStats state `mapWithIndex` state.players)
      , HH.ul
        [ HP.class_ cssClass.play ]
        (HH.li_ [(HH.h3 [] [ HH.text $ "Play" ])]
        : (renderCard (const Nothing) currentPlayer <$> currentPlayer.atPlay))
      , HH.ul
        [ HP.class_ cssClass.buying ]
        (HH.li_ [(HH.h3 [] [ HH.text $ "Buying" ])]
        : (renderCard (const Nothing) currentPlayer <$> currentPlayer.buying))
      , renderHand player playerIndex state
      ]

renderHand :: forall a. Player -> Int -> GameState -> HTML a GameAction
renderHand player playerIndex state = HH.ul
  [ HP.classes $
    [ cssClass.hand
    , if state.turn == playerIndex && state.phase == ActionPhase
      then cssClass.active
      else cssClass.inactive
    ] <> if player.actions < 1 || (length $ isAction `filter` player.hand) < 1
      then [ cssClass.waiting ]
      else []
  ] $
  [ HH.li_ [ HH.h3 [] [ HH.text $ "Hand" ] ]
  , HH.li_
    [ HH.ul_
      [ HH.li [ HP.class_ cssClass.handInfo ] [ HH.text $ (show $ player.actions) <> " Actions" ]
      , HH.li [ HP.class_ cssClass.handInfo ] [ HH.text $ "$" <> (show ((cash player) :: Int)) ]
      , HH.li [ HP.class_ cssClass.handInfo ] [ HH.text $ (show $ player.buys) <> " Buys" ]
      ]
    ]
  , renderDiscard player
  ] <> renderCardInHand player playerIndex `mapWithIndex` player.hand
  <> [ renderDeck player ]

renderCardInHand :: forall a. Player -> Int -> Int -> Card -> HTML a GameAction
renderCardInHand player playerIndex cardIndex card =
  renderCard (\_ -> Just $ Play playerIndex cardIndex) player card

--handleAction :: forall m. MonadEffect m => Int -> GameAction -> HalogenM GameState GameAction () GameState m Unit
handleAction playerIndex = case _ of
  ResolveChoice choice -> do
    H.modify_ \state -> fromMaybe state (resolveChoice playerIndex choice state)
  Receive (NewGame n) -> do
    setup (newGame n) >>= H.put
    untilJust autoAdvance
    H.get >>= H.raise
  Receive (UpdateState gs) -> H.put gs
  NextPhase playerIndex -> do
    gameState <- H.get
    maybeNewGameState <- nextPhase playerIndex gameState
    H.modify_ \oldGameState ->
      fromMaybe oldGameState { text = "Error: not your turn!" } maybeNewGameState
    untilJust autoAdvance
    H.get >>= H.raise
  Play player card -> do
    gameState <- H.get
    maybeNewGameState <- play player card gameState
    case maybeNewGameState of
      Nothing -> H.modify_ \state -> state { text = "Error" }
      Just newGameState -> H.put newGameState
    untilJust autoAdvance
    H.get >>= H.raise
  Purchase playerIndex player stack -> do
    gameState <- H.get
    H.modify_ \oldGameState ->
        case purchase playerIndex player stack gameState of
          Nothing -> oldGameState { text = "Error trying to buy card!" }
          Just newGameState -> newGameState
    untilJust autoAdvance
    H.get >>= H.raise

autoAdvance :: forall m. MonadEffect m => MonadState GameState m => m (Maybe Unit)
autoAdvance = do
  gameState <- H.get
  liftEffect (Console.log $ "autoAdvance? from " <> show gameState.phase)
  case gameState.players !! gameState.turn of
    Nothing -> pure $ Just unit -- TODO: return an error
    Just player -> case gameState.phase of
      ActionPhase -> if hasActions player && hasActionCardsInHand player
        then pure $ Just unit
        else advancePhase
      BuyPhase -> if player.buys > 0
        then pure $ Just unit
        else advancePhase
      CleanupPhase -> advancePhase
  where
    advancePhase :: m (Maybe Unit)
    advancePhase = do
      gameState <- H.get
      liftEffect $ Console.log $ "advancing from " <> show gameState.phase
      mbNewState <- nextPhase gameState.turn gameState
      case mbNewState of
        Nothing -> pure (Just unit)
        Just newGameState -> const Nothing <$> H.put newGameState

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
  , toTrash: H.ClassName "to-keep"
  , toKeep: H.ClassName "to-trash"
  }

