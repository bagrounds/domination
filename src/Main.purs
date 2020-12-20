module Main where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapWithIndex, take, (:))
import Data.Either (Either(..))
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

import Dominion (Card, GameState, Player, Stack, newGame, nextPhase, play, purchase, score, value)
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
render state = HH.div_
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
  renderCardInSupply playerIndex player <$> state.supply

renderCardInSupply :: forall a. Int -> Player -> Stack -> HTML a AppAction
renderCardInSupply playerIndex player stack = HH.button
  [ HE.onClick \_ -> Just $ PlayGame $ Purchase playerIndex player stack ]
  [ HH.text $ "(" <> show stack.count <> ") " <> stack.card.name <> " $" <> show stack.card.cost ]

renderPlayers :: forall a. GameState -> Array (HTML a AppAction)
renderPlayers state =
  [ HH.p [] [ HH.text $ "Turn: Player " <> show state.turn ]
  , HH.p [] [ HH.text $ "Phase: " <> show state.phase ]
  ] <> renderPlayer state `mapWithIndex` state.players

renderPlayer :: forall a. GameState -> Int -> Player -> HTML a AppAction
renderPlayer state playerIndex player = HH.div_
  [ HH.h2 []
    [ HH.text
    $ "Player " <> show playerIndex <> " (VP: " <> show (score player) <> ")"
    <> " Actions: " <> show player.actions <> " Buys: " <> show player.buys
    ]
  , HH.button [ HE.onClick \_ -> Just $ PlayGame $ NextPhase playerIndex ] [ HH.text "Next Phase" ]
  , HH.div_ ((HH.text $ "Supply ($" <> show (value player.hand + value player.atPlay - value player.buying) <> ") ")
    : renderSupply playerIndex player state)
  , HH.div_ (HH.text "Deck " : (renderCardView <$> player.deck))
  , HH.div_ (HH.text "Hand " : renderCardInHand playerIndex `mapWithIndex` player.hand)
  , HH.div_ (HH.text "Play " : (renderCardView <$> player.atPlay))
  , HH.div_ (HH.text "Buying " : (renderCardView <$> player.buying))
  , HH.div_ (HH.text "Discard " : (renderCardView <$> player.discard))
  ]

renderCardView :: forall a. Card -> HTML a AppAction
renderCardView card = HH.button [] [HH.text card.name]

renderCardInHand :: forall a. Int -> Int -> Card -> HTML a AppAction
renderCardInHand playerIndex cardIndex card = HH.button
  [ HE.onClick \_ -> Just $ PlayGame $ Play playerIndex cardIndex ]
  [ HH.text card.name ]

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
    liftEffect $ Comm.log rd
    H.modify_ \state -> state { offer = rd }
  MakeAnswer -> do
    s <- H.get
    ld <- liftAff $ makeAff $ Comm.join s.offer Right
    H.modify_ \state -> state { answer = ld }
  WriteAnswer rd -> do
    liftEffect $ Comm.log rd
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
          liftEffect $ Comm.log gs
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
        NewGame -> H.modify_ _ { gameState = newGame }
        NextPhase playerIndex -> H.modify_ \state ->
          case (nextPhase playerIndex state.gameState) of
            Nothing -> state { text = "Error: not your turn!" }
            Just gameState -> state { gameState = gameState }
        Play player card -> H.modify_ \state ->
          case play player card state.gameState of
            Nothing -> state { text = "Error" }
            Just gameState -> state { gameState = gameState }
        Purchase playerIndex player stack -> H.modify_ \state ->
          case purchase playerIndex player stack state.gameState of
            Nothing -> state { text = "Error trying to buy card!" }
            Just gameState -> state { gameState = gameState, text = "good" }

