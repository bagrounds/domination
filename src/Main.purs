module Main where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (intercalate, mapWithIndex, take, (:), filter)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (EventType(..), Event)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent (MouseEvent)

import Dominion (Card, GameState, Player, Stack, Phase(..), setup, newGame, nextPhase, play, purchase, score, value, cash, isAction, isTreasure, isVictory)
import Comm as Comm

type AppState =
  { chatInputMessage :: String
  , messages :: Array String
  , offer :: String
  , answer :: String
  , receivedAnswer :: String
  , message :: String
  , localDescription :: String
  , gameState :: GameState
  , text :: String
  }

newApp :: AppState
newApp =
  { chatInputMessage: ""
  , messages: []
  , offer: ""
  , answer: ""
  , receivedAnswer: ""
  , message: ""
  , localDescription: ""
  , gameState: newGame
  , text: ""
  }

main :: Effect Unit
main = HA.runHalogenAff (runUI component unit =<< HA.awaitBody)

component :: forall m a b c. MonadAff m => Component HTML a b c m
component = H.mkComponent { eval, initialState, render } where
  eval = H.mkEval (H.defaultEval { handleAction = handleAction })
  initialState = const newApp

render :: forall b. AppState -> HTML b AppAction
render state = HH.main_
  [ HH.div [ HP.id_ "msg", HE.handler (EventType "msg") (Just <<< ReceiveMessage) ] []
  , HH.h1 [] [ HH.text "Creator" ]
  , HH.button [ HE.onClick \_ -> Just MakeOffer ] [ HH.text "MakeOffer" ]
  , HH.text state.localDescription
  , HH.input
    [ HP.type_ HP.InputText
    , HP.placeholder "put joiner's answer here"
    , HP.required true
    , HE.onValueInput $ Just <<< WriteAnswer
    ]
  , HH.button [ HE.onClick \_ -> Just AcceptAnswer ] [ HH.text "Got Answer" ]
  , HH.h1 [] [ HH.text "Joiner" ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.placeholder "put creator's offer here"
    , HP.required true
    , HE.onValueInput $ Just <<< WriteOffer
    ]
  , HH.button [ HE.onClick \_ -> Just MakeAnswer ] [ HH.text "Join" ]
  , HH.text state.answer
  , HH.h1 [] [ HH.text "Chat" ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.value state.chatInputMessage
    , HP.required true
    , HE.onValueInput $ Just <<< WriteMessage
    , HE.onKeyDown \e -> if (KE.key e) == "Enter" then Just SendMessage else Nothing
    ]
  , HH.button [ HE.onClick \_ -> Just SendMessage ] [ HH.text "Send" ]
  , HH.div_ $ (\m -> HH.p [] [ HH.text m ]) <$> (take 5 state.messages)
  , HH.h1 [] [ HH.text "Domination" ]
  , HH.button [ HE.onClick \_ -> Just $ PlayGame NewGame ] [ HH.text "New Game" ]
  , HH.div_ $ renderPlayers state.gameState
  , HH.div_ [ HH.h2 [] [ HH.text "Game State" ] ]
  , HH.div_ [ HH.text $ show state ]
  ]

renderSupply :: forall a. Int -> Player -> GameState -> Array (HTML a AppAction)
renderSupply playerIndex player state =
  renderStack playerIndex player <$> state.supply

renderDeck :: forall a. Player -> HTML a AppAction
renderDeck player = HH.button
  [ HE.onClick (const Nothing), HP.class_ prop.class.deck ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Discard" ]
    , HH.li [] [ HH.text $ show ((length player.discard) :: Int) ]
    ]
  ]

renderDiscard :: forall a. Player -> HTML a AppAction
renderDiscard player = HH.button
  [ HE.onClick (const Nothing), HP.class_ prop.class.discard ]
  [ HH.ul_
    [ HH.li [] [ HH.text "Deck" ]
    , HH.li [] [ HH.text $ show ((length player.deck) :: Int) ]
    ]
  ]

renderCard :: forall a. (MouseEvent -> Maybe AppAction) -> Player -> Card -> HTML a AppAction
renderCard onClick player card = HH.li
  (if isTreasure card then [ HP.class_ prop.class.treasureCard ] else [ HP.class_ prop.class.noTreasureCard ])
  [ HH.li (if isVictory card then [ HP.class_ prop.class.victoryCard ] else [ HP.class_ prop.class.noVictoryCard ])
    [ HH.li (if isAction card then [ HP.class_ prop.class.actionCard ] else [ HP.class_ prop.class.noActionCard ])
      [ HH.button
        [ HE.onClick onClick
        , HP.classes
          $ (if player.actions > 0 && isAction card then [ prop.class.canPlay ] else [ prop.class.cantPlay ])

          <> [ prop.class.card ]
        ]
        [ HH.ul_
          [ HH.li [ HP.classes [ prop.class.cardText, prop.class.cardName ] ] [ HH.text $ " " <> card.name ]
          , HH.li [ HP.classes [ prop.class.cardText, prop.class.cardCards ] ] [ HH.text (if card.cards > 0 then " +" <> show card.cards <> " Card" else "") ]
          , HH.li [ HP.classes [ prop.class.cardText, prop.class.cardActions ] ] [ HH.text $ (if card.actions > 0 then " +" <> show card.actions <> " Action" else "") ]
          , HH.li [ HP.classes [ prop.class.cardText, prop.class.cardBuys ] ] [ HH.text (if card.buys > 0 then " +" <> show card.buys <> " Buy" else "") ]
          , HH.li [ HP.classes [ prop.class.cardText, prop.class.cardTreasure ] ] [ HH.text (if card.treasure > 0 then " +$" <> show card.treasure else "") ]
          , HH.li [ HP.classes [ prop.class.cardText, prop.class.cardVictoryPoints ] ] [ HH.text (if card.victoryPoints > 0 then " +" <> show card.victoryPoints <> " VP" else "") ]
          , HH.li
            [ HP.classes [ prop.class.cardText, prop.class.cardCost ] ]
            [ HH.text $ "Cost $" <> show card.cost ]
          ]
        ]
      ]
    ]
  ]

renderStack :: forall a. Int -> Player -> Stack -> HTML a AppAction
renderStack playerIndex player stack =
  HH.li [ HP.class_ prop.class.stack ]
    [ HH.ul_
      [ HH.li [ HP.class_ prop.class.stackCount ] [ HH.text $ "(" <> show stack.count <> ")" ]
      , HH.li
        [ HP.classes
          [ prop.class.stackCard
          , if player.buys > 0 && cash player >= stack.card.cost && stack.count > 0 then prop.class.canBuy else prop.class.cantBuy
          ]
        ]
        [ renderCard (\_ -> Just $ PlayGame $ Purchase playerIndex player stack) player stack.card ]
      ]
    ]

renderPlayers :: forall a. GameState -> Array (HTML a AppAction)
renderPlayers state =
  [ HH.p [] [ HH.text $ "Turn: Player " <> show state.turn ]
  ] <> renderPlayer state `mapWithIndex` state.players

--playerStats :: GameState -> Int -> Player -> String
playerStats state playerIndex player = HH.li
  [ HP.class_ prop.class.stat ]
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

renderPlayer :: forall a. GameState -> Int -> Player -> HTML a AppAction
renderPlayer state playerIndex player = HH.div_
  [ HH.ul [ HP.class_ prop.class.stats ] (playerStats state `mapWithIndex` state.players)
  , HH.button [ HE.onClick \_ -> Just $ PlayGame $ NextPhase playerIndex ] [ HH.text "Next Phase" ]
  , HH.ul
      [ HP.classes
        [ prop.class.supply
        , if state.turn == playerIndex && state.phase == BuyPhase then prop.class.active else prop.class.inactive
        ]
      ]
    (HH.li_ [(HH.h3 [] [ HH.text $ "Supply" ])] : renderSupply playerIndex player state)
  , HH.ul
    [ HP.classes
      [ prop.class.hand
      , if state.turn == playerIndex && state.phase == ActionPhase then prop.class.active else prop.class.inactive
      ]
    ]
    (HH.li_ [(HH.h3 [] [ HH.text $ "Hand" ])]
    : renderDiscard player : renderCardInHand player playerIndex `mapWithIndex` player.hand <> [ renderDeck player ])
  , HH.ul [ HP.class_ prop.class.play ] (HH.li_ [(HH.h3 [] [ HH.text $ "Play" ])] : (renderCard (const Nothing) player <$> player.atPlay))
  , HH.ul [ HP.class_ prop.class.buying ] (HH.li_ [(HH.h3 [] [ HH.text $ "Buying" ])] : (renderCard (const Nothing) player <$> player.buying))
  ]

prop =
  { class:
    { stats: H.ClassName "stats"
    , stat: H.ClassName "stat"
    , supply: H.ClassName "supply"
    , hand: H.ClassName "hand"
    , play: H.ClassName "play"
    , buying: H.ClassName "buying"
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
    , inactive: H.ClassName "inactive"
    , canBuy: H.ClassName "can-buy"
    , cantBuy: H.ClassName "cant-buy"
    , canPlay: H.ClassName "can-play"
    , cantPlay: H.ClassName "cant-play"
    }
  }

renderCardInHand :: forall a. Player -> Int -> Int -> Card -> HTML a AppAction
renderCardInHand player playerIndex cardIndex card =
  renderCard (\_ -> Just $ PlayGame $ Play playerIndex cardIndex) player card

data AppAction = MakeOffer
  | WriteOffer String
  | MakeAnswer
  | WriteAnswer String
  | AcceptAnswer
  | SendMessage
  | WriteMessage String
  | ReceiveMessage Event
  | PlayGame GameAction

data GameAction = NewGame
  | NextPhase Int
  | Play Int Int
  | Purchase Int Player Stack

data Message = ChatMessage String | GameStateMessage GameState
derive instance genericMessage :: Generic Message _
instance encodeJsonMessage :: EncodeJson Message where
  encodeJson = genericEncodeJson
instance decodeJsonMessage :: DecodeJson Message where
  decodeJson = genericDecodeJson
readMessage :: String -> Either String Message
readMessage = decodeJson <=< jsonParser
writeMessage :: Message -> String
writeMessage = stringify <<< encodeJson

handleAction :: forall m. MonadState AppState m
  => MonadAff m
  => MonadEffect m
  => AppAction -> m Unit
handleAction = case _ of
  MakeOffer -> do
    ld <- liftAff $ makeAff $ Comm.create Right
    H.modify_ _ { localDescription = ld }
  WriteOffer rd -> do
    H.modify_ \state -> state { offer = rd }
  MakeAnswer -> do
    s <- H.get
    ld <- liftAff $ makeAff $ Comm.join s.offer Right
    H.modify_ \state -> state { answer = ld }
  WriteAnswer rd -> do
    H.modify_ \state -> state { receivedAnswer = rd }
  AcceptAnswer -> do
    s <- H.get
    liftEffect $ Comm.gotAnswer s.receivedAnswer
  WriteMessage s -> do
    H.modify_ \state -> state { chatInputMessage = s }
  SendMessage -> do
    s <- H.get
    sendMessage $ writeMessage $ ChatMessage s.chatInputMessage
    let localMessage = "-> " <> s.chatInputMessage
    H.modify_ \state -> state { messages = localMessage : state.messages, chatInputMessage = "" }
  ReceiveMessage customEvent -> do
    let msg = readMessage $ Comm.detail customEvent
    case msg of
      Left e -> liftEffect $ Comm.log e
      Right mt -> case mt of
        GameStateMessage gs -> do
          H.modify_ \state -> state { gameState = gs, messages = "(incoming game state)" : state.messages }
        ChatMessage message -> do
          let remoteMessage = "<- " <> message
          H.modify_ \state -> state { messages = remoteMessage : state.messages }
  PlayGame gameAction -> do
    handleGameAction gameAction
    s <- H.get
    sendMessage $ writeMessage $ GameStateMessage s.gameState
  where
    sendMessage = liftEffect <<< Comm.say
    handleGameAction gameAction =
      case gameAction of
        NewGame -> do
          gameState <- setup newGame
          H.modify_ _{ gameState = gameState }
        NextPhase playerIndex -> do
          state <- H.get
          maybeNewGameState <- nextPhase playerIndex state.gameState
          H.modify_ \state ->
            case maybeNewGameState of
              Nothing -> state { text = "Error: not your turn!" }
              Just gameState -> state { gameState = gameState }
        Play player card -> do
          state <- H.get
          maybeNewGameState <- play player card state.gameState
          case maybeNewGameState of
            Nothing -> H.modify_ \state -> state { text = "Error" }
            Just gameState -> H.modify_ \state -> state { gameState = gameState }

        Purchase playerIndex player stack -> H.modify_ \state ->
          case purchase playerIndex player stack state.gameState of
            Nothing -> state { text = "Error trying to buy card!" }
            Just gameState -> state { gameState = gameState, text = "good" }

