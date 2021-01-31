module Main where

import Prelude

import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set, (.~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Domination.AppM (runAppM)
import Domination.Capability.Broadcast (class Broadcast, Broadcaster, broadcast, create, runBroadcastM)
import Domination.Capability.GenUuid (genUuid, runGenUuidM)
import Domination.Capability.Log (class Log, log, runLogM)
import Domination.Capability.Random (class Random, randomElement, runRandomM)
import Domination.Capability.Storage (class Storage, load, runStorageM, save)
import Domination.UI.Chat as Chat
import Domination.UI.Domination (GameEvent(..), GameQuery(..))
import Domination.UI.Domination as Domination
import Domination.UI.UsernameInput as UsernameInput
import Domination.UI.Util (h1__)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import FFI as FFI
import Halogen (Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (HalogenM)
import Halogen.VDom.Driver (runUI)
import Message (Message(..), Envelope)
import Util (prependOver, readJson, writeJson, (:~))
import Web.Event.Event (Event, EventType(..))

type AppState =
  { connectionCount :: Int
  , id :: String
  , username :: String
  , usernames :: HashMap String String
  , message :: String
  , messages :: Array Message
  , gameOn :: Boolean
  , broadcaster :: Broadcaster
  , roomCode :: String
  }

_messages :: Lens' AppState (Array Message)
_messages = prop (SProxy :: SProxy "messages")
_message :: Lens' AppState String
_message = prop (SProxy :: SProxy "message")
_connectionCount :: Lens' AppState Int
_connectionCount = prop (SProxy :: SProxy "connectionCount")
_usernames :: Lens' AppState (HashMap String String)
_usernames = prop (SProxy :: SProxy "usernames")
_username :: Lens' AppState String
_username = prop (SProxy :: SProxy "username")

newApp :: Broadcaster -> String -> String -> HashMap String String -> AppState
newApp broadcaster username uuid usernames =
  { connectionCount: 0
  , id: uuid
  , username: username
  , usernames
  , message: ""
  , messages: []
  , gameOn: false
  , broadcaster: broadcaster
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
      uuid <- runGenUuidM genUuid
      runStorageM $ save uuidKey uuid
      pure $ show uuid
    Right uuid -> pure uuid

  body <- HA.awaitBody

  username <- case eUsername of
    Left e -> do
      runLogM $ log "no existing username found, using default"
      emoji <- fromMaybe ":)" <$> runRandomM (randomElement emojis)
      pure $ emoji <> "lurker" <> emoji
    Right u -> pure u

  let usernames = HashMap.insert uuid username HashMap.empty

  broadcaster <- runBroadcastM $ create globalRoomCode
  let initialState = newApp broadcaster username uuid usernames
  runUI (root initialState) unit body

root :: forall s query o. AppState -> Component HTML query o s Aff
root state = H.hoist (runAppM {}) $ component state

component
  :: forall m s query o
  . Storage m
  => Log m
  => Random m
  => Broadcast m
  => AppState
  -> Component HTML query o s m
component state = H.mkComponent { eval, initialState, render } where
  eval = H.mkEval H.defaultEval
    { handleAction = handleAction
    , initialize = Just $ HandleGameEvent LoadGame
    }
  initialState _ = state

render
  :: forall c m
  . Log m
  => Random m
  => AppState
  -> HTML (Domination.Component GameQuery c m AppAction) AppAction
render state = HH.main_ $
  [ HH.div
    [ HP.id_ "msg"
    , HE.handler (EventType "msg") (Just <<< ReceiveMessage)
    ]
    []
  , UsernameInput.render { onInput: WriteUsername, state }
  , h1__ $ show state.connectionCount <> " Users Connected"
  , Chat.render
    { sendEvent: SendMessage
    , onInput: Write _message
    , state
    }
  , HH.slot
    Domination._component
    0
    Domination.component
    unit
    (Just <<< HandleGameEvent)
  ]

data AppAction
  = WriteUsername String
  | Write (Lens' AppState String) String
  | SendMessage
  | ReceiveMessage Event
  | HandleGameEvent GameEvent

type ChildComponents o r =
  ("Domination" :: H.Slot GameQuery o Int | r)

handleAction
  :: forall output m t1 r
  . Storage m
  => Log m
  => Broadcast m
  => AppAction -> HalogenM AppState AppAction (ChildComponents t1 r ) output m Unit
handleAction = case _ of
  WriteUsername username -> do
    { id, usernames } <- H.get
    save "username" username
    sendMessage $ UsernameMessage { username, id }
    H.modify_ $ set _username username
      <<< over _usernames (HashMap.insert id username)
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
            H.modify_ $ _messages :~ msg
            if message == "PING"
              then do
                H.modify_ $ _message .~ "PONG"
                sendChatMessage
              else pure unit
          GameStateMessage state -> do
            log "Receive GameStateMessage"
            queryGame $ ReceiveGameState state
          PlayMadeMessage _ -> do
            log $ "Receive PlayMadeMessage"
            H.modify_ $ _messages :~ msg
  HandleGameEvent gameEvent -> case gameEvent of
    NewState activeState -> do
      sendMessage $ GameStateMessage activeState.state
      log $ "saving state as player" <> show activeState.playerIndex
      save "game_state" activeState
      log $ "Main: NewState"
    PlayMade x -> do
      log $ "Main: PlayMade"
      let message = PlayMadeMessage x
      sendMessage message
      H.modify_ $ prependOver _messages message
    LoadGame -> do
      log $ "Main: LoadGame"
      mbGameState <- load "game_state"
      case mbGameState of
        Left e -> log e
        Right activeState -> do
          log $ "Main: LoadGame successful as player"
            <> show activeState.playerIndex
          queryGame $ LoadActiveState activeState
          sendMessage $ GameStateMessage activeState.state
    SaveGame activeState -> do
      log $ "saving state as player" <> show activeState.playerIndex
      save "game_state" activeState
  where
    queryGame state = do
      _ <- H.query Domination._component 0 (state unit)
      pure unit
    sendChatMessage = do
      { id, message } <- H.get
      let chat = ChatMessage { username: id, message }
      sendMessage chat
      H.modify_ $ (_message .~ "") <<< (_messages :~ chat)

    sendMessage message = do
      log "Main: sending message"
      { broadcaster, id } <- H.get
      broadcast broadcaster $ writeJson { id, message }

