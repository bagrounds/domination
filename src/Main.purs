module Main where

import Prelude

import Control.Monad.State.Class (gets)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Int (toNumber)
import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Domination.AppM (runAppM)
import Domination.Capability.Log (class Log, error, log, runLogM)
import Domination.Capability.Storage (class Storage, load, runStorageM, save)
import Domination.UI.Chat as Chat
import Domination.UI.Css as Css
import Domination.UI.Domination (GameEvent(..), GameUpdate(..))
import Domination.UI.Domination as Domination
import Domination.UI.UsernameInput as UsernameInput
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
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
import Message (Message(..), Envelope)
import Util (prependOver, randomElement, readJson, writeJson)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, toEvent)

type AppState =
  { connectionCount :: Int
  , id :: String
  , username :: String
  , usernames :: HashMap String String
  , playerCount :: Int
  , playerIndex :: Int
  , chatInputMessage :: String
  , messages :: Array Message
  , message :: String
  , localDescription :: String
  , gameOn :: Boolean
  , gameState :: Maybe GameUpdate
  , text :: String
  , bugout :: Bugout
  , roomCode :: String
  }

_gameState :: Lens' AppState (Maybe GameUpdate)
_gameState = prop (SProxy :: SProxy "gameState")
_messages :: Lens' AppState (Array Message)
_messages = prop (SProxy :: SProxy "messages")
_playerIndex :: Lens' AppState Int
_playerIndex = prop (SProxy :: SProxy "playerIndex")
_playerCount :: Lens' AppState Int
_playerCount = prop (SProxy :: SProxy "playerCount")
_chatInputMessage :: Lens' AppState String
_chatInputMessage = prop (SProxy :: SProxy "chatInputMessage")
_connectionCount :: Lens' AppState Int
_connectionCount = prop (SProxy :: SProxy "connectionCount")
_usernames :: Lens' AppState (HashMap String String)
_usernames = prop (SProxy :: SProxy "usernames")
_username :: Lens' AppState String
_username = prop (SProxy :: SProxy "username")

newApp :: Bugout -> String -> String -> HashMap String String -> AppState
newApp bugout username uuid usernames =
  { connectionCount: 0
  , id: uuid
  , username: username
  , usernames
  , playerCount: 1
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
main = launchAff_ $ do
  eUuid <- runStorageM $ load uuidKey
  eUsername <- runStorageM $ load usernameKey
  uuid <- case eUuid of
    Left e -> do
      runLogM $ log "no existing uuid found, generating a new one"
      uuid <- liftEffect $ FFI.genUuid
      runStorageM $ save uuidKey uuid
      pure $ show uuid
    Right uuid -> pure uuid

  body <- HA.awaitBody

  username <- case eUsername of
    Left e -> do
      runLogM $ log "no existing username found, using default"
      emoji <- fromMaybe ":)" <$> randomElement emojis
      pure $ emoji <> "lurker" <> emoji
    Right u -> pure u

  let usernames = HashMap.insert uuid username HashMap.empty

  bugout <- makeAff $ FFI.makeBugout globalRoomCode Left Right
  let initialState = newApp bugout username uuid usernames
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
  eval = H.mkEval H.defaultEval { handleAction = handleAction }
  initialState _ = state

type DominationComponent o c m a =
  ComponentSlot HTML ("Domination" :: Slot o GameEvent Int | c) m AppAction

preventTyping
  :: forall x a
  . HP.IProp (onKeyDown :: KeyboardEvent | x) AppAction
preventTyping = HE.onKeyDown \e -> Just $ PreventDefault (toEvent e)

incrementer
  :: forall w a
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
  => AppState -> HTML (DominationComponent o c m String) AppAction
render state = HH.main_ $
  [ HH.div [ HP.id_ "msg", HE.handler (EventType "msg") (Just <<< ReceiveMessage) ] []
  , UsernameInput.render { onInput: WriteUsername, state }
  , HH.h1 [] [ HH.text $ show state.connectionCount <> " Users Connected" ]
  , Chat.render
    { sendEvent: SendMessage
    , onInput: Write _chatInputMessage
    , state
    }
  , HH.div
    [ HP.class_ $ ClassName "container" ]
    [ HH.label_ [ HH.text "Players: " ]
    , incrementer (Just 1) Nothing state.playerCount WritePlayerCount
    ]
  , HH.div
    [ HP.class_ $ ClassName "container" ]
    [ HH.label_ [ HH.text "Player #: " ]
    , incrementer (Just 1) Nothing (state.playerIndex + 1) ((_ - 1) >>> WritePlayerIndex)
    ]
  , HH.div_
    [ HH.button
      [ HE.onClick \_ -> Just $ StartNewGame ]
      [ HH.text $ "Start New " <> show state.playerCount
        <> " Player Game as Player " <> show (state.playerIndex + 1)
      ]
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
          UpdateState { state, playerIndex } ->
            Domination.component (length state.players) playerIndex
          MakeNewGame { playerCount, playerIndex } ->
            Domination.component playerCount playerIndex
        gameUpdate
        (Just <<< UpdateGameState)
      ]

data AppAction
  = WriteUsername String
  | Write (Lens' AppState String) String
  | WritePlayerIndex Int
  | WritePlayerCount Int
  | SendMessage
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
  PreventDefault e ->
    liftEffect $ preventDefault e
  WriteUsername username -> do
    { id, usernames } <- H.get
    save "username" username
    sendMessage $ UsernameMessage { username, id }
    H.modify_ $ set _username username
      <<< over _usernames (HashMap.insert id username)
  WritePlayerIndex index -> do
    { playerIndex, playerCount } <- H.get
    log $ "updating playerIndex from "
      <> show playerIndex <> " -> " <> show index
    H.modify_ $ set _playerIndex (max index 0)
      <<< set _playerCount (max (index + 1) playerCount)
  WritePlayerCount count -> do
    { playerIndex, playerCount } <- H.get
    log $ "updating playerCount from "
      <> show playerCount <> " -> " <> show count
    H.modify_ $ set _playerCount (max count 1)
      <<< set _playerIndex (min (count - 1) playerIndex)
  Write lens value -> H.modify_ $ set lens value
  SendMessage -> sendChatMessage
  ReceiveMessage customEvent ->
    case readJson $ FFI.detail customEvent of
      Left e -> log $ "problem receiving message: " <> e
      Right ({ message: msg } :: Envelope) -> do
        case msg of
          UsernameMessage { username, id } -> do
            log $ "username incoming: " <> username
            H.modify_ $ over _usernames (HashMap.insert id username)
          SeenMessage address -> do
            log $ "I see you: " <> address
            { username, id } <- H.get
            sendMessage $ UsernameMessage { username, id }
          ConnectionsMessage count ->
            H.modify_ $ set _connectionCount count
          ChatMessage { message, username } -> do
            H.modify_ $ prependOver _messages msg
            if message == "PING"
              then do
                H.modify_ $ set _chatInputMessage "PONG"
                sendChatMessage
              else pure unit
          GameStateMessage state -> do
            log "Receive GameStateMessage"
            { playerIndex } <- H.get
            H.modify_ $ set _gameState $ Just
              $ UpdateState { state, playerIndex }
          PlayMadeMessage _ -> do
            log "Receive PlayMadeMessage"
            H.modify_ $ prependOver _messages msg
  LoadGame -> do
    mbGameState <- load "game_state"
    case mbGameState of
      Left e -> log e
      Right state -> do
        { playerIndex } <- H.get
        H.modify_ $ set _gameState $ Just
          $ UpdateState { state, playerIndex }
        sendMessage $ GameStateMessage state
  StartNewGame -> do
    log "StartNewGame"
    { playerIndex, playerCount } <- H.get
    H.modify_ $ set _gameState $ Just
      $ MakeNewGame { playerCount, playerIndex }
  UpdateGameState event -> case event of
    NewState state -> do
      { playerIndex } <- H.get
      H.modify_ $ set _gameState $ Just
        $ UpdateState { state, playerIndex }
      sendMessage $ GameStateMessage state
      save "game_state" state
      log $ "NewState"
    PlayMade play -> do
      log $ "PlayMade: " <> show play
      { gameState } <- H.get
      case gameState of
        Just (UpdateState { state, playerIndex }) -> do
          log $ "UpdateState" <> show play
          let message = PlayMadeMessage { play, playerIndex, state }
          sendMessage message
          H.modify_ $ prependOver _messages message
        Just (MakeNewGame { playerCount, playerIndex }) ->
          error "I didn't realize this could happen!"
        Nothing -> pure unit
  where
    sendChatMessage = do
      { id, chatInputMessage } <- H.get
      let
        message = ChatMessage
          { username: id
          , message: chatInputMessage
          }
      sendMessage message
      H.modify_ $ set _chatInputMessage ""
        <<< prependOver _messages message

    sendMessage message = do
      log "Main: sending message"
      bugout <- gets _.bugout
      id <- H.gets _.id
      let package = { id, message }
      liftEffect $ FFI.send bugout $ writeJson package

