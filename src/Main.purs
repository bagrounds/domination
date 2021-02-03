module Main where

import Prelude

import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Lens.Getter (view)
import Data.Lens.Lens (Lens')
import Data.Lens.Prism (review)
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set, (%~), (.~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Domination.AppM (runAppM)
import Domination.Capability.Broadcast (class Broadcast, Broadcaster, broadcast, create)
import Domination.Capability.Dom (class Dom)
import Domination.Capability.GenUuid (class GenUuid, genUuid)
import Domination.Capability.Log (class Log, error, log)
import Domination.Capability.Random (class Random, randomElement)
import Domination.Capability.Storage (class Storage, load, save)
import Domination.Capability.WireCodec (class WireCodec, readWire, writeWire)
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
import Message (Message(..), WireEnvelope, WireMessage)
import Message as Message
import Util ((:~))
import Web.Event.Event (Event, EventType(..))

type AppState =
  { connectionCount :: Int
  , id :: String
  , username :: String
  , usernames :: HashMap String String
  , message :: String
  , messages :: Array Message
  , gameOn :: Boolean
  , maybeBroadcaster :: Maybe Broadcaster
  , roomCode :: String
  }

_id :: Lens' AppState String
_id = prop (SProxy :: SProxy "id")
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
_maybeBroadcaster :: Lens' AppState (Maybe Broadcaster)
_maybeBroadcaster = prop (SProxy :: SProxy "maybeBroadcaster")

newApp :: AppState
newApp =
  { connectionCount: 0
  , id: ""
  , username: ""
  , usernames: HashMap.empty
  , message: ""
  , messages: []
  , gameOn: false
  , maybeBroadcaster: Nothing
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
  body <- HA.awaitBody
  runUI root unit body

root :: forall s query o. Component HTML query o s Aff
root = H.hoist (runAppM {}) component

component
  :: forall m s query o
  . Storage m
  => Dom m
  => Log m
  => Random m
  => GenUuid m
  => Broadcast m
  => WireCodec m
  => Component HTML query o s m
component = H.mkComponent { eval, initialState, render } where
  eval = H.mkEval H.defaultEval
    { handleAction = handleAction
    , initialize = Just $ Initialize
    }
  initialState _ = newApp

render
  :: forall c m
  . Log m
  => Dom m
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
  = Initialize
  | WriteUsername String
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
  => GenUuid m
  => Random m
  => Broadcast m
  => WireCodec m
  => AppAction -> HalogenM AppState AppAction (ChildComponents t1 r ) output m Unit
handleAction = case _ of
  Initialize -> do
    eUuid <- load uuidKey
    uuid <- case eUuid of
      Left e -> do
        log "no existing uuid found, generating a new one"
        uuid <- genUuid
        save uuidKey uuid
        pure $ show uuid
      Right uuid -> pure uuid

    eUsername <- load usernameKey
    username <- case eUsername of
      Left e -> do
        log "no existing username found, using default"
        emoji <- fromMaybe ":)" <$> randomElement emojis
        pure $ emoji <> "lurker" <> emoji
      Right u -> pure u

    broadcaster <- create globalRoomCode

    H.modify_ $ (_id .~ uuid)
      >>> (_username .~ username)
      >>> (_usernames %~ HashMap.insert uuid username)
      >>> (_maybeBroadcaster .~ Just broadcaster)

    loadGame "game_state"

  WriteUsername username -> do
    { id, usernames } <- H.get
    save "username" username
    sendMessage $ UsernameMessage { username, id }
    H.modify_ $ set _username username
      <<< over _usernames (HashMap.insert id username)
  Write lens value -> H.modify_ $ set lens value
  SendMessage -> sendChatMessage
  ReceiveMessage customEvent -> do
    let detail = FFI.detail customEvent
    log $ "ReceiveMessage: " <> detail
    (eWireEnvelope :: Either String WireEnvelope) <- readWire detail
    let
      eMessage = (
        rmap
        (review Message._toWire))
        <$> eWireEnvelope
    case eMessage of
      Left e -> log $ "problem receiving message: " <> e
      Right (Tuple _ (msg :: Message)) -> do
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
          GameStateMessage { i, state, playMade } -> do
            log "Main: Receive GameStateMessage"
            queryGame $ ReceiveGameState
              { i
              , state
              }
            case playMade of
              Just x -> do
                log $ "Receive PlayMadeMessage"
                H.modify_ $ _messages :~ PlayMadeMessage x
              Nothing -> pure unit
          PlayMadeMessage _ ->
            error "PlayMadeMessage should not be called"
  HandleGameEvent gameEvent -> case gameEvent of
    NewState activeState playMade -> do
      sendMessage $ GameStateMessage
        { state: activeState.state
        , i: activeState.i
        , playMade
        }
      case playMade of
        Just x -> do
          log $ "Main: PlayMade"
          H.modify_ $ _messages :~ (PlayMadeMessage x)
        Nothing -> pure unit
      log $ "saving state as player" <> show activeState.playerIndex
      saveGame activeState
      queryGame $ LoadActiveState activeState
      log $ "Main: NewState"
    LoadGame -> loadGame "game_state"
    SaveGame activeState -> saveGame activeState
    Undo { i } -> do
      let
        saveNumber = (i - 1) `mod` 10
        key = "game_state_" <> show saveNumber
      loadGame key
  where
    loadGame key = do
      log $ "Main: LoadGame"
      mbGameState <- load key
      case mbGameState of
        Left e -> log e
        Right activeState -> do
          log $ "Main: LoadGame successful as player"
            <> show activeState.playerIndex
          queryGame $ LoadActiveState activeState
          sendMessage $ GameStateMessage
            { state: activeState.state
            , i: activeState.i
            , playMade: Nothing
            }
    saveGame activeState = do
      log $ "saving state as player" <> show activeState.playerIndex
      let
        saveNumber = activeState.i `mod` 10
        key = "game_state_" <> show saveNumber
      save key activeState
      save "game_state" activeState
    queryGame state = do
      _ <- H.query Domination._component 0 (state unit)
      pure unit
    sendChatMessage = do
      { id, message } <- H.get
      let (chat :: Message) = ChatMessage { username: id, message }
      sendMessage chat
      H.modify_ $ (_message .~ "") <<< (_messages :~ chat)

    sendMessage
      :: Message
      -> HalogenM AppState AppAction (ChildComponents t1 r ) output m Unit
    sendMessage message' = do
      let (message :: WireMessage) = view Message._toWire message'
      log "Main: sending message"
      { maybeBroadcaster, id } <- H.get
      let (wireEnvelope :: WireEnvelope) = Tuple id message
      case maybeBroadcaster of
        Nothing -> do
          error "no broadcaster; cannot send message"
        Just broadcaster -> do
          log $ "Encoding message: " <> show wireEnvelope
          (eString :: Either String String) <- writeWire wireEnvelope
          case eString of
            Left e -> error e
            Right string -> do
              log $ "Sending message: " <> string
              broadcast broadcaster string

