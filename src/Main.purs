module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Halogen.Query.HalogenM (HalogenM)
import Data.Either (Either(..))
import Data.Array (mapWithIndex, take, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Halogen.Query.EventSource as ES
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
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall m a b c. MonadAff m => Component HTML a b c m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: b -> AppState
  initialState _ = newApp

render :: forall b. AppState -> HTML b AppAction
render state =
  HH.div_
    [ HH.div [ HP.id_ "msg", HE.handler (EventType "msg") (\e -> Just $ MessageEventReceived e) ] []
    , HH.h1 [] [ HH.text "Creator" ]
    , HH.button [ HE.onClick \_ -> Just Create ] [ HH.text "Create" ]
    , HH.text state.localDescription
    , HH.input
      [ HP.type_ HP.InputText
      , HP.value "put joiner's answer here"
      , HP.required true
      , HE.onValueInput $ Just <<< SetAnswer
      ]
    , HH.button [ HE.onClick \_ -> Just GotAnswer ] [ HH.text "Got Answer" ]
    , HH.h1 [] [ HH.text "Joiner" ]
    , HH.input
      [ HP.type_ HP.InputText
      , HP.value "put creater's offer here"
      , HP.required true
      , HE.onValueInput $ Just <<< SetOffer
      ]
    , HH.button [ HE.onClick \_ -> Just Join ] [ HH.text "Join" ]
    , HH.text state.answer
    , HH.h1 [] [ HH.text "Chat" ]
    , HH.input
      [ HP.type_ HP.InputText
      , HP.value state.chatInputMessage
      , HP.required true
      , HE.onValueInput $ Just <<< SetMessage
      , HE.onKeyDown \e -> if (KE.key e) == "Enter" then Just SendMessage else Nothing
      ]
    , HH.button [ HE.onClick \_ -> Just SendMessage ] [ HH.text "Send" ]
    , HH.div_ $ map (\m -> HH.p [] [ HH.text m ]) (take 5 state.messages)
    , HH.h1 [] [ HH.text "Domination" ]
    , HH.button [ HE.onClick \_ -> Just $ GameAction' NewGame ] [ HH.text "New Game" ]
    , HH.div_ $ renderPlayers state.gameState
    , HH.div_ [ HH.h2 [] [ HH.text "Game State" ] ]
    , HH.div_ [ HH.text $ show state ]
    ]

renderSupply :: forall a. Int -> Player -> GameState -> Array (HTML a AppAction)
renderSupply playerIndex player state =
  map (renderCardInSupply playerIndex player) state.supply

renderCardInSupply :: forall a. Int -> Player -> Stack -> HTML a AppAction
renderCardInSupply playerIndex player stack =
  HH.button
    [ HE.onClick \_ -> Just $ GameAction' $ Purchase playerIndex player stack ]
    [ HH.text $ "(" <> show stack.count <> ") " <> stack.card.name <> " $" <> show stack.card.cost]

renderPlayers :: forall a. GameState -> Array (HTML a AppAction)
renderPlayers state =
  [ HH.p [] [ HH.text $ "Turn: Player " <> show state.turn ]
  , HH.p [] [ HH.text $ "Phase: " <> show state.phase ]
  ]
  <> (renderPlayer state) `mapWithIndex` state.players

renderPlayer :: forall a. GameState -> Int -> Player -> HTML a AppAction
renderPlayer state playerIndex player = HH.div_
  [ HH.h2 [] [ HH.text $ "Player " <> (show playerIndex) <> " (VP: " <> show (score player) <> ")" ]
  , HH.button [ HE.onClick \_ -> Just $ GameAction' $ NextPhase playerIndex ] [ HH.text "Next Phase" ]
  , HH.h3 [] [ HH.text $ "Supply ($" <> show (value player.atPlay - value player.buying) <> ")"  ]
  , HH.div_ $ renderSupply playerIndex player state
  , HH.h3 [] [ HH.text $ "Deck" ]
  , HH.div_ $ map renderCardView player.deck
  , HH.h3 [] [ HH.text $ "Cards in Hand" ]
  , HH.div_ $ mapWithIndex renderCardInHand (map (\x -> Tuple playerIndex x) player.hand)
  , HH.h3 [] [ HH.text $ "Cards at Play" ]
  , HH.div_ $ map renderCardView player.atPlay
  , HH.h3 [] [ HH.text $ "Discard" ]
  , HH.div_ $ map renderCardView player.discard
  ]

renderCardView :: forall a. Card -> HTML a AppAction
renderCardView card = HH.button [] [HH.text card.name]

renderCardInHand :: forall a. Int -> Tuple Int Card -> HTML a AppAction
renderCardInHand cardIndex (Tuple playerIndex card) = HH.button [HE.onClick \_ -> Just $ GameAction' $ Play playerIndex cardIndex] [ HH.text card.name ]

data AppAction =
  Create
  | Join
  | GotAnswer
  | SendMessage
  | SetMessage String
  | MessageEventReceived Event
  | SetOffer String
  | SetAnswer String
  | GameAction' GameAction

data GameAction =
  NewGame
  | NextPhase Int
  | Play Int Int
  | Purchase Int Player Stack

data MessageType = Chat String | GameState' GameState
derive instance genericMessageType :: Generic MessageType _
instance encodeJsonMessageType :: EncodeJson MessageType where
  encodeJson a = genericEncodeJson a
instance decodeJsonMessageType :: DecodeJson MessageType where
  decodeJson a = genericDecodeJson a
readMessageType :: String -> Either String MessageType
readMessageType = decodeJson <=< jsonParser
writeMessageType :: MessageType -> String
writeMessageType = stringify <<< encodeJson

type Setup = ES.Emitter Effect AppAction -> Effect (ES.Finalizer Effect)
handleAction :: forall o m. MonadAff m
  => AppAction
  -> HalogenM AppState AppAction () o m Unit
handleAction = case _ of
  Create -> do
    ld <- liftAff $ makeAff $ (Comm.create Right)
    H.modify_ _ { localDescription = ld }
  MessageEventReceived customEvent -> do
    let msg = readMessageType $ Comm.detail $ customEvent
    case msg of
      Left e -> liftEffect $ Comm.log e
      Right mt -> case mt of
        GameState' gs -> do
          liftEffect $ Comm.log gs
          H.modify_ \state -> state { gameState = gs, messages = "(incoming game state)" : state.messages }
        Chat message -> do
          let remoteMessage = "<- " <> message
          H.modify_ \state -> state { messages = remoteMessage : state.messages }
  Join -> do
    s <- H.get
    ld <- liftAff $ makeAff $ Comm.join s.offer Right
    H.modify_ \state -> state { answer = ld }
  SetAnswer rd -> do
    liftEffect $ Comm.log rd
    H.modify_ \state -> state { receivedAnswer = rd }
  GotAnswer -> do
    s <- H.get
    liftEffect $ Comm.gotAnswer s.receivedAnswer
  SetMessage s -> do
    H.modify_ \state -> state { chatInputMessage = s }
  SendMessage -> do
    s <- H.get
    sendMessage $ writeMessageType $ Chat s.chatInputMessage
    let localMessage = "-> " <> s.chatInputMessage
    H.modify_ \state -> state { messages = localMessage : state.messages, chatInputMessage = "" }
  SetOffer rd -> do
    liftEffect $ Comm.log rd
    H.modify_ \state -> state { offer = rd }
  GameAction' gameAction -> do
    handleGameAction gameAction
    s <- H.get
    sendMessage $ writeMessageType $ GameState' s.gameState
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

