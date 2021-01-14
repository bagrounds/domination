module Main where

import Prelude

import Control.Monad.State.Class (gets)
import Data.Array (take, (:))
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Domination.AppM (runAppM)
import Domination.Capability.Log (class Log, error, log)
import Domination.Capability.Storage (class Storage, load, save)
import Domination.UI.Css as Css
import Domination.UI.Domination (GameEvent(..), GameUpdate(..))
import Domination.UI.Domination as Domination
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import FFI (Bugout)
import FFI as FFI
import Halogen (Component, ComponentSlot, Slot)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)
import Halogen.VDom.Driver (runUI)
import Message (Message(..))
import Message as Message
import Util (randomElement, readJson, writeJson)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, toEvent)
import Web.UIEvent.KeyboardEvent as KE

type AppState =
  { connectionCount :: Int
  , id :: String
  , username :: String
  , usernameMap :: HashMap String String
  , players :: Int
  , playerIndex :: Int
  , chatInputMessage :: String
  , messages :: Array Message
  , message :: String
  , localDescription :: String
  , gameOn :: Boolean
  , gameState :: Maybe Domination.GameUpdate
  , text :: String
  , bugout :: Bugout
  , roomCode :: String
  }

newApp :: Bugout -> String -> String -> HashMap String String -> AppState
newApp bugout username uuid usernameMap =
  { connectionCount: 0
  , id: uuid
  , username: username
  , usernameMap
  , players: 1
  , playerIndex: 0
  , chatInputMessage: ""
  , messages: []
  , message: ""
  , localDescription: ""
  , gameOn: false
  , gameState: Nothing
  , text: ""
  , bugout: bugout
  , roomCode: globalRoomCode
  }

globalRoomCode :: String
globalRoomCode = "global"

uuidKey :: String
uuidKey = "player-id"

usernameKey :: String
usernameKey = "username"

emojis :: Array String
emojis = ["😄","😃","😀","😊","☺","😉","😍","😘","😚","😗","😙","😜","😝","😛","😳","😁","😔","😌","😒","😞","😣","😢","😂","😭","😪","😥","😰","😅","😓","😩","😫","😨","😱","😠","😡","😤","😖","😆","😋","😷","😎","😴","😵","😲","😟","😦","😧","😈","👿","😮","😬","😐","😕","😯","😶","😇","😏","😑","👲","👳","👮","👷","💂","👶","👦","👧","👨","👩","👴","👵","👱","👼","👸","😺","😸","😻","😽","😼","🙀","😿","😹","😾"]

main :: Effect Unit
main =  HA.runHalogenAff $ do
  eUuid <- liftAff $ runAppM {} $ load uuidKey
  eUsername :: Either String String <- liftAff $ runAppM {} $ load usernameKey
  uuid <- case eUuid of
    Left e -> do
      liftAff $ runAppM {} $ log "no existing uuid found, generating a new one"
      uuid <- liftEffect $ FFI.genUuid
      liftAff $ runAppM {} $ save uuidKey uuid
      pure $ show uuid
    Right uuid -> pure uuid

  body <- HA.awaitBody

  username <- case eUsername of
    Left e -> do
      liftAff $ runAppM {} $ log "no existing username found, using default"
      emoji <- fromMaybe ":)" <$> randomElement emojis
      pure $ emoji <> "lurker" <> emoji
    Right u -> pure u

  let usernameMap = HashMap.insert uuid username HashMap.empty

  bugout :: Bugout <- makeAff $ FFI.makeBugout globalRoomCode Left Right
  let (initialState :: AppState) = newApp bugout username uuid usernameMap
  runUI (root initialState) (MakeNewGame { playerCount: 1, playerIndex: 0 }) body

root :: forall s query o. AppState -> Component HTML query o s Aff
root state = H.hoist (runAppM {}) $ component state

component
  :: forall m s query o
  . Storage m
  => Log m
  => MonadEffect m
  => AppState -> Component HTML query o s m
component state = H.mkComponent { eval, initialState, render } where
  eval = H.mkEval (H.defaultEval { handleAction = handleAction })
  initialState _ = state

type DominationComponent o c m =
  ComponentSlot HTML ("Domination" :: Slot o GameEvent Int | c) m AppAction

preventTyping :: forall x. HP.IProp (onKeyDown :: KeyboardEvent | x) AppAction
preventTyping = HE.onKeyDown \e -> Just $ PreventDefault (toEvent e)

incrementer
  :: forall w
  . Maybe Int -> Maybe Int -> Int -> (Int -> AppAction) -> HTML w AppAction
incrementer mbMin mbMax value setValue = HH.div
  [ HP.class_ $ Css.incrementer ]
  [ HH.button
    [ HE.onClick \_ -> case mbMin of
      Just min ->
        if value <= min
        then Just $ setValue min
        else Just $ setValue (value - 1)
      Nothing -> Just $ setValue (value - 1)
    ] [ HH.text "-" ]
  , HH.input $
    [ HP.value $ show value
    , HP.required true
    , preventTyping
    ]
    <> case mbMin of
      Just min -> [ HP.min $ toNumber min ]
      Nothing -> []
    <> case mbMin of
      Just max -> [ HP.max $ toNumber max ]
      Nothing -> []
  , HH.button
    [ HE.onClick $ \_ -> case mbMax of
      Just max ->
        if value >= max
        then Just $ setValue max
        else Just $ setValue (value + 1)
      Nothing -> Just $ setValue (value + 1)
    ]
    [ HH.text "+" ]
  ]

render
  :: forall o c m
  . Log m
  => MonadEffect m
  => AppState -> HTML (DominationComponent o c m) AppAction
render state = HH.main_ $
  [ HH.div [ HP.id_ "msg", HE.handler (EventType "msg") (Just <<< ReceiveMessage) ] []
  , HH.label_ [ HH.text "Username: " ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.value state.username
    , HP.placeholder usernameKey
    , HP.required true
    , HE.onValueInput $ Just <<< WriteUsername
    ]
  , HH.h1 [] [ HH.text $ show state.connectionCount <> " Users in Chat" ]
  , HH.div_
    [ HH.div
      [ HP.class_ $ ClassName "chat-history" ]
      $ Message.renderHtml <$>
        let um = state.usernameMap in
        let f = \x ->
              case x of
                ChatMessage { username, message } -> ChatMessage { message, username: fromMaybe username $ HashMap.lookup username um }
                y -> y in
        let messages = f <$> take 1000 state.messages
        in messages
    , HH.div
      [ HP.class_ $ ClassName "chat-form"]
      [ HH.button
        [ HE.onClick \_ -> Just SendMessage
        , HP.class_ $ ClassName "send-chat"
        ]
        [ HH.text "Send" ]
      , HH.span_
        [ HH.input
          [ HP.type_ HP.InputText
          , HP.class_ $ ClassName "chat-input"
          , HP.value state.chatInputMessage
          , HP.required true
          , HE.onValueInput $ Just <<< WriteMessage
          , HE.onKeyDown \e -> if (KE.key e) == "Enter" then Just SendMessage else Nothing
          ]
        ]
      ]
    ]
  , HH.div
    [ HP.class_ $ ClassName "container" ]
    [ HH.label_ [ HH.text "Players: " ]
    , incrementer (Just 1) Nothing state.players WritePlayerCount
    ]
  , HH.div
    [ HP.class_ $ ClassName "container" ]
    [ HH.label_ [ HH.text "Player #: " ]
    , incrementer (Just 1) Nothing (state.playerIndex + 1) ((_ - 1) >>> WritePlayerIndex)
    ]
  , HH.div_
    [ HH.button
      [ HE.onClick \_ -> Just $ StartNewGame ]
      [ HH.text $ "Start New " <> show state.players <> " Player Game as Player " <> show (state.playerIndex + 1) ]
    , HH.button
      [ HE.onClick \_ -> Just $ LoadGame ]
      [ HH.text "Load Game" ]
    ]
  ] <> case state.gameState of
    Nothing -> []
    Just gameUpdate ->
      [ HH.slot
        (SProxy :: SProxy "Domination")
        0
        case gameUpdate of
          UpdateState { state, playerIndex } -> (Domination.component (length state.players) playerIndex)
          MakeNewGame { playerCount, playerIndex } -> Domination.component playerCount playerIndex
        gameUpdate
        (Just <<< UpdateGameState)
      ]

data AppAction
  = CopyToClipboard String
  | WriteUsername String
  | WritePlayerIndex Int
  | WritePlayerCount Int
  | SendMessage
  | WriteMessage String
  | ReceiveMessage Event
  | LoadGame
  | StartNewGame
  | UpdateGameState GameEvent
  | PreventDefault Event

handleAction
  :: forall slots output m
  . Storage m
  => Log m
  => MonadEffect m
  => AppAction -> HalogenM AppState AppAction slots output m Unit
handleAction = case _ of
  PreventDefault e -> liftEffect $ preventDefault e
  CopyToClipboard id -> liftEffect $ FFI.copyToClipboard id
  WriteUsername username -> do
    s <- H.get
    let (m :: Message) = UsernameMessage { username, id: s.id }

    save "username" username
    sendMessage m
    let um = HashMap.insert s.id username s.usernameMap
    H.modify_ \state -> state { username = username, usernameMap = um }
  WritePlayerIndex playerIndex -> do
    state <- H.get
    log $ "updating playerIndex from "
      <> show state.playerIndex
      <> " -> "
      <> show playerIndex
    if playerIndex > 0
    then H.modify_ _{ playerIndex = playerIndex }
    else H.modify_ \s -> s { playerIndex = 0 }
    if playerIndex >= state.players
    then H.modify_ _{ players = playerIndex + 1 }
    else pure unit
  WritePlayerCount players -> do
      state <- H.get
      if players > 1
      then H.modify_ _{ players = players }
      else H.modify_ \s -> s { players = 1 }
      if players <= state.playerIndex
      then H.modify_ _{ playerIndex = players - 1 }
      else pure unit
  WriteMessage s -> do
    H.modify_ \state -> state { chatInputMessage = s }
  SendMessage -> sendChatMessage
  ReceiveMessage customEvent -> do
    let (ePackage :: Either String Message.Envelope) = readJson $ FFI.detail customEvent
    case ePackage of
      Left e -> log $ "problem receiving message: " <> e
      Right { id, message: mt } -> do
        case mt of
            UsernameMessage { username, id: i } -> do
              s <- H.get
              let usernameMap = HashMap.insert i username s.usernameMap
              H.put s { usernameMap = usernameMap }

              log $ "username incoming "
              log $ "username map: " <> show usernameMap
            SeenMessage address -> do
              log $ "I see you: " <> address
              { username, id } <- H.get
              sendMessage $ UsernameMessage { username, id }
            ConnectionsMessage count ->
              H.modify_ \state -> state { connectionCount = count }
            ChatMessage { message, username } -> do
              H.modify_ \state -> state { messages = mt : state.messages }
              if message == "PING"
                then do
                  H.modify_ _ { chatInputMessage = "PONG" }
                  sendChatMessage
                else pure unit
            GameStateMessage gs -> do
              log "Receive GameStateMessage"
              H.modify_ \state -> state { gameState = Just $ UpdateState { state: gs, playerIndex: state.playerIndex } }
            PlayMadeMessage _ -> do
              log $ "Receive PlayMadeMessage"
              H.modify_ \state -> state { messages = mt : state.messages }
  LoadGame -> do
    mbGameState <- load "game_state"
    case mbGameState of
      Left e -> log e
      Right gameState -> do
        H.modify_ \state -> state { gameState = Just $ UpdateState { state: gameState, playerIndex: state.playerIndex } }
        sendMessage $ GameStateMessage gameState
  StartNewGame -> do
    state <- H.get
    log "StartNewGame"
    H.modify_ _ { gameState = Just $ MakeNewGame { playerCount: state.players, playerIndex: state.playerIndex } }
  UpdateGameState event -> case event of
    NewState gameState -> do
      H.modify_ \state -> state { gameState = Just $ UpdateState { state: gameState, playerIndex: state.playerIndex } }
      sendMessage $ GameStateMessage gameState
      save "game_state" gameState
      log $ "NewState"
    PlayMade play -> do
      s <- H.get
      case s.gameState of
        Just (UpdateState { state, playerIndex }) -> do
          let message = PlayMadeMessage { play, player: playerIndex, state }
          sendMessage message
          H.modify_ \state -> state { messages = message : state.messages }
          log $ "UpdateState" <> show play
        Just (MakeNewGame { playerCount, playerIndex }) ->
          error "I didn't realize this could happen!"
        Nothing -> pure unit
      log $ "PlayMade: " <> show play
  where
    sendChatMessage = do
      s <- H.get
      let message = ChatMessage { username: s.id, message: s.chatInputMessage }
      sendMessage message
      H.modify_ \state -> state { messages = message : state.messages, chatInputMessage = "" }

    sendMessage message = do
      log "Main: sending message"
      bugout <- gets _.bugout
      id :: String <- H.gets _.id
      let package = { id, message }
      liftEffect $ FFI.send bugout $ writeJson package

