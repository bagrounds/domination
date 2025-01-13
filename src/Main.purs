--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| This is a Haskell implementation of a chat application using the Halogen library for building reactive user interfaces. Here's a breakdown of the code:
--|
--| **Overview**
--|
--| The code defines a `sendMessage` function that takes a `RemoteMessage` as input and sends it to a broadcast channel. The message is encoded into a wire envelope, which is then sent over a network connection.
--|
--| **Key Data Structures**
--|
--| * `AppState`: represents the application state, including the chat messages, player index, and other game-related data.
--| * `AppAction`: represents the actions that can be performed on the application state (e.g., sending a message).
--| * `ChildComponents`: represents the child components of the application UI.
--|
--| **Functions**
--|
--| * `sendMessage`: takes a `RemoteMessage` as input and sends it to a broadcast channel. It encodes the message into a wire envelope, which is then sent over a network connection.
--| * `maybeCreateBroadcaster`: creates a new broadcaster if one doesn't exist.
--| * `writeWire`: writes a wire envelope to a string.
--|
--| **Main Logic**
--|
--| 1. Get the current announcement from the application state (`announce`).
--| 2. Create or retrieve a broadcaster (if it exists) and associate it with the current broadcast target.
--| 3. Encode the message into a wire envelope using `writeWire`.
--| 4. If encoding fails, log an error and exit.
--| 5. Broadcast the encoded string over the network connection.
--|
--| **Notes**
--|
--| * The `RemoteMessage` type is not defined in this code snippet, so I assume it's defined elsewhere in the application.
--| * The `maybeCreateBroadcaster` function creates a new broadcaster with the given room code, remote message target, and local message target. This suggests that there are multiple broadcast channels available for different use cases (e.g., chat room, public announcement).
--| * The `writeWire` function takes a wire envelope as input and returns a string representation of it. This is used to encode the message into a format that can be sent over the network.
--| * The `broadcast` function sends the encoded string over the network connection.
--|
--| Overall, this code snippet demonstrates how to send a message over a network connection using Halogen and a broadcaster mechanism.
--|
--| ### Key Concepts
--| This is a Halogen state machine in Haskell. It appears to be a game or chat application that allows users to send and receive messages. Here's an overview of the code:
--|
--| **Main Functions**
--|
--| * `sendMessage`: This function takes a `RemoteMessage` as input, extracts its wire-encoded form, and broadcasts it using the configured broadcaster.
--| * `loadGame`: This function loads a previously saved game state from disk and upgrades it to the current state.
--| * `saveGame`: This function saves the current game state to disk.
--|
--| **Data Structures**
--|
--| * `RemoteMessage`: A message sent over the network, which can be either a chat message or a game-related event (e.g., new state or save).
--| * `WireCodec m`: A type alias for a wire codec, which is used to encode and decode messages.
--| * `Broadcast m`: A type alias for a broadcaster, which is an object that can broadcast messages.
--|
--| **State Machine**
--|
--| The state machine is defined using the Halogen library. The `App` type represents the application's state, which includes the current game state (`activeState`) and other metadata (e.g., player index).
--|
--| The main components of the state machine are:
--|
--| * `_messages`: A list of recent messages, which are used to display chat history.
--| * `_message`: The current message being displayed.
--| * `_chatNumber`: The current chat number.
--| * `_playerIndex`: The current player index.
--|
--| **Event Handling**
--|
--| The state machine responds to various events, including:
--|
--| * `RemoteMessage`: When a new remote message is received, the broadcaster is updated, and the message is broadcast using the `sendMessage` function.
--| * `NewState`: When a new game state is received, the current state is upgraded using the `loadGame` function.
--|
--| **Logging**
--|
--| The code uses logging to display error messages or chat history. The `logErrorToChat` function takes an error message as input and displays it in the chat interface.
--|
--| Overall, this code appears to be a simple chat application that allows users to send and receive messages, with some additional features for saving and loading game states.

module Main where

import Prelude

import AppAction (AppAction(..))
import AppState (AppState, CardSpecSelection, _announce, _chatNumber, _connectionCount, _dominationConfig, _id, _kingdom, _longGame, _maybeAudioContext, _maybeBroadcaster, _message, _messages, _nextPlayerCount, _nextPlayerIndex, _showMenu, _username, _usernames, defaultAnnounce, defaultKingdom, newApp, upgradeSelection)
import Audio.WebAudio.Types (AudioContext)
import Control.Monad.State (class MonadState)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Array (elem, length, take)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.HashMap as HashMap
import Data.Lens.Getter (view)
import Data.Lens.Prism (review)
import Data.Lens.Setter (over, set, (%~), (.~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Domination.AppM (runAppM)
import Domination.Capability.Audio (class Audio, newAudioContext, runAudioM)
import Domination.Capability.Broadcast (class Broadcast, broadcast, maybeCreateBroadcaster)
import Domination.Capability.Dom (class Dom)
import Domination.Capability.GenUuid (class GenUuid, genUuid)
import Domination.Capability.Log (class Log, error, log)
import Domination.Capability.Random (class Random, randomElement, shuffle)
import Domination.Capability.Storage (class Storage, load, save)
import Domination.Capability.WireCodec (class WireCodec, readWire, writeWire)
import Domination.Data.Card (CardSpec)
import Domination.UI.Chat as Chat
import Domination.UI.Css as Css
import Domination.UI.DomSlot (Area(..), DomSlot(..))
import Domination.UI.Domination (GameQuery(..))
import Domination.UI.Domination as Domination
import Domination.UI.Domination.ActiveState (_playerIndex)
import Domination.UI.Domination.ActiveState as ActiveState
import Domination.UI.Domination.GameEvent (GameEvent(..))
import Domination.UI.Icons as Icons
import Domination.UI.Settings as Settings
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Emojis (emojis)
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
import Message (LocalMessage(..), RemoteMessage(..), WireEnvelope)
import Message as Message
import Util ((:~))
import Web.Event.Event (EventType(..))

remoteMessageTarget :: String
remoteMessageTarget = "remote-message-target"

localMessageTarget :: String
localMessageTarget = "local-message-target"

uuidKey :: String
uuidKey = "player-id"

announceKey :: String
announceKey = "announce"

usernameKey :: String
usernameKey = "username"

chatKey :: String
chatKey = "chat"

main :: Effect Unit
main = launchAff_ $ do
  liftEffect $ FFI.registerServiceWorker
  audioContext <- liftEffect $ runAudioM newAudioContext
  HA.awaitLoad
  body <- HA.awaitBody
  runUI (root audioContext) unit body

root :: forall s query o. AudioContext -> Component query o s Aff
root audioContext = H.hoist (runAppM {}) (component audioContext)

component
  :: forall m s query o
  . Storage m
  => Dom m
  => Log m
  => Random m
  => GenUuid m
  => Broadcast m
  => WireCodec m
  => Audio m
  => AudioContext
  -> Component query o s m
component audioContext =
  H.mkComponent { eval, initialState, render: render audioContext }
  where
  eval = H.mkEval H.defaultEval
    { handleAction = handleAction audioContext
    , initialize = Just Initialize
    }
  initialState _ = newApp

render
  :: forall c m
  . Log m
  => Audio m
  => Storage m
  => Dom m
  => Random m
  => AudioContext
  -> AppState
  -> HTML (Domination.Component GameQuery c m AppAction) AppAction
render audioContext state = HH.main_ $
  [ HH.div
    [ HP.id $ remoteMessageTarget
    , HE.handler (EventType "purescript") ReceiveRemoteMessage
    ]
    []
  , HH.div
    [ HP.id $ localMessageTarget
    , HE.handler
      (EventType "purescript")
      (ReceiveLocalMessage)
    ]
    []
  , if state.showMenu
    then Settings.render state
    else Settings.renderEmpty
  , renderSettingsButton
  , HH.slot
      Domination._component
      (AreaSlot GameArea)
      (Domination.component state.dominationConfig audioContext)
      unit
      HandleGameEvent
  , Chat.render
    { sendEvent: SendMessage
    , onInput: Write _message
    , nothing: DoNothing
    , state
    }
  , HH.i
    [ HP.class_ Css.connections ]
    [ HH.text $ show state.connectionCount ]
  ]

renderSettingsButton :: forall w. HTML w AppAction
renderSettingsButton = HH.button
  [ HP.class_ Css.settingsButton
  , HH.attr (H.AttrName "aria-label") "Settings"
  , HE.onClick \_ -> ToggleMenu
  ]
  [ Icons.settings ]

type ChildComponents o r q1 o1 =
  ( "Domination" :: H.Slot GameQuery o DomSlot
  , "description" :: H.Slot q1 o1 DomSlot
  | r
  )

saveChat
  :: forall m r messages
  . Monad m
  => MonadState { messages :: messages | r } m
  => Storage m
  => EncodeJson messages
  => DecodeJson messages
  => Log m
  => m (Either String Unit)
saveChat = do
  messages <- H.gets _.messages
  result <- save chatKey messages
  log "saved chat messages"
  pure result


handleAction
  :: forall output m t1 r o1 q1
  . Audio m
  => Storage m
  => Log m
  => GenUuid m
  => Random m
  => Broadcast m
  => WireCodec m
  => AudioContext
  -> AppAction
  -> HalogenM AppState AppAction (ChildComponents t1 r o1 q1) output m Unit
handleAction audioContext = case _ of
  Initialize -> do
    log "Initialize"
    eUuid <- load uuidKey
    uuid <- case eUuid of
      Left e -> do
        log $ "Initialize: no existing uuid found, generating a new one."
          <> " error: " <> e
        uuid <- genUuid
        save uuidKey uuid >>= logErrorToChat
        pure $ show uuid
      Right uuid -> pure uuid

    eUsername <- load usernameKey
    username <- case eUsername of
      Left e -> do
        log $ "Initialize: no existing username found, using default."
          <> " error: " <> e
        emoji <- fromMaybe ":)" <$> randomElement emojis
        pure $ emoji <> "lurker" <> emoji
      Right u -> pure u

    roomCode <- H.gets _.roomCode

    eKingdom <- load "kingdom"
    kingdom <- case eKingdom of
      Left e -> do
        log $ "Initialize: Failed to load kingdom. Falling back to default."
          <> "Error: " <> e
        pure defaultKingdom
      Right k ->
        if length k == length defaultKingdom
        then pure $ upgradeSelection <$> k
        else pure defaultKingdom

    ePlayerIndex <- load "player_index"
    nextPlayerIndex <- case ePlayerIndex of
      Left e -> do
        log $ "Initialize: Failed to load playerIndex. Falling back to default."
          <> "Error: " <> e
        pure 0
      Right i -> pure i

    ePlayerCount <- load "player_count"
    nextPlayerCount <- case ePlayerCount of
      Left e -> do
        log $ "Initialize: Failed to load playerCount. Falling back to default."
          <> "Error: " <> e
        pure 1
      Right i -> pure i

    eMessages <- load chatKey
    messages <- case eMessages of
      Left e -> do
        log $ "Initialize: Failed to load messages. Error: " <> e
        pure []
      Right m -> pure m

    log "Initialize: modify a bunch of stuff..."
    H.modify_ $ (_id .~ uuid)
      >>> (_username .~ username)
      >>> (_usernames %~ HashMap.insert uuid username)
      >>> (_dominationConfig <<< _kingdom .~ kingdom)
      >>> (_dominationConfig <<< _nextPlayerIndex .~ nextPlayerIndex)
      >>> (_dominationConfig <<< _nextPlayerCount .~ nextPlayerCount)
      >>> (_maybeAudioContext .~ Just audioContext)
      >>> (_messages .~ messages)
    log "Initialize: done modifying a bunch of stuff..."

    log "Initialize: load game..."
    loadGame "game_state"
    log "Initialize: done loading game..."

    eAnnounce <- load announceKey
    announce <- case eAnnounce of
      Left e -> do
        log $ "Initialize: no existing announce found, using default."
          <> " error: " <> e
        pure $ defaultAnnounce
      Right u -> pure u

    log $ "Initialize: announce: " <> announce
    H.modify_ (_announce .~ announce)

    maybeBroadcaster <- maybeCreateBroadcaster
      roomCode remoteMessageTarget localMessageTarget announce

    log $ "Initialize: broadcaster: " <> show maybeBroadcaster

    H.modify_ (_maybeBroadcaster .~ maybeBroadcaster)

    broadcaster <- H.gets _.maybeBroadcaster

    log $ "Initialize: broadcaster: " <> show broadcaster

  ToggleMenu -> H.modify_ $ _showMenu %~ not

  WritePlayerIndex index -> do
    { dominationConfig: { nextPlayerCount } } <- H.get
    let
      newPlayerIndex = max index zero
      newPlayerCount = max (index + one) nextPlayerCount
    H.modify_ $ (_dominationConfig <<< _nextPlayerIndex .~ newPlayerIndex)
      >>> (_dominationConfig <<< _nextPlayerCount .~ newPlayerCount)
    save "player_index" newPlayerIndex >>= logErrorToChat
    save "player_count" newPlayerCount >>= logErrorToChat

  WritePlayerCount count -> do
    { nextPlayerIndex } <- H.gets _.dominationConfig
    let
      newPlayerIndex = min (count - one) nextPlayerIndex
      newPlayerCount = max count one
    H.modify_
      $ (_dominationConfig <<< _nextPlayerCount .~ newPlayerCount)
      >>> (_dominationConfig <<< _nextPlayerIndex .~ newPlayerIndex)
    save "player_index" newPlayerIndex >>= logErrorToChat
    save "player_count" newPlayerCount >>= logErrorToChat

  RandomizeKingdom -> do
    { kingdom } <- H.gets _.dominationConfig
    shuffledKingdom <- shuffle kingdom
    let
      cardsToKeep = take 16 $ _.cardSpec <$> shuffledKingdom
      newKingdom = selectIfElement cardsToKeep <$> kingdom
    H.modify_ $ _dominationConfig <<< _kingdom .~ newKingdom
    save "kingdom" newKingdom >>= logErrorToChat
    where
      selectIfElement :: Array CardSpec -> CardSpecSelection -> CardSpecSelection
      selectIfElement keepers { cardSpec } =
        { cardSpec, selected: cardSpec `elem` keepers }

  ChooseKingdom kingdom -> do
    save "kingdom" kingdom >>= logErrorToChat
    H.modify_ $ _dominationConfig <<< _kingdom .~ kingdom

  ToggleLongGame ->
    H.modify_ $ _dominationConfig <<< _longGame %~ not

  DoNothing -> pure unit

  StartNewGame -> do
    config <- H.gets _.dominationConfig
    queryGame $ StartNewGameRequest config
    H.modify_ $ _showMenu .~ false

  LoadGameRequest -> do
    loadGame "game_state"
    H.modify_ $ _showMenu .~ false

  WriteAnnounce announce -> do
    save announceKey announce >>= logErrorToChat
    log $ "saving announce: " <> announce
    H.modify_ $ set _announce announce

  WriteUsername username -> do
    { id } <- H.get
    save usernameKey username >>= logErrorToChat
    H.modify_ $ set _username username
      <<< over _usernames (HashMap.insert id username)
    sendMessage $ UsernameMessage { username, id }

  Write lens value -> H.modify_ $ set lens value
  SendMessage -> sendChatMessage
  ReceiveLocalMessage customEvent -> do
    let localMessage = FFI.detail customEvent
    case localMessage of
      SeenMessage address -> do
        log $ "I see you: " <> address
        { username, id } <- H.get
        sendMessage $ UsernameMessage { username, id }
      ConnectionsMessage count ->
        H.modify_ $ set _connectionCount count

  ReceiveRemoteMessage customEvent -> do
    let detail = FFI.detail customEvent
    (eWireEnvelope :: Either String WireEnvelope) <- readWire detail
    let
      eMessage = (
        rmap
        (review Message._toWire))
        <$> eWireEnvelope
    case eMessage of
      Left e -> error $ "problem receiving message: " <> e
      Right (Tuple _ msg) -> do
        case msg of
          UsernameMessage { username, id } -> do
            log $ "username incoming: " <> username
            H.modify_ $ over _usernames
              $ HashMap.insert id username
          ChatMessage { message } -> do
            H.modify_
              $ (_messages :~ msg)
              >>> (_messages %~ take 250)
            saveChat >>= logErrorToChat
            if message == "PING"
              then do
                H.modify_ $ _message .~ "PONG"
                sendChatMessage
              else pure unit
          GameMessage { i, state, playMade } -> do
            queryGame $ ReceiveGame
              { i
              , state
              }
            case playMade of
              Just x -> do
                H.modify_ $ _messages :~ PlayMadeMessage x
              Nothing -> pure unit
          PlayMadeMessage _ ->
            error "PlayMadeMessage should not be called"
  HandleGameEvent gameEvent -> case gameEvent of
    NewState activeState playMade -> do
      case playMade of
        Just x -> do
          H.modify_ $ _messages :~ (PlayMadeMessage x)
        Nothing -> pure unit
      queryGame $ LoadActiveState activeState
      sendMessage $ GameMessage
        { state: activeState.state
        , i: activeState.i
        , playMade
        }
      saveGame activeState
    SaveGame activeState -> saveGame activeState
    Undo { i } -> do
      let
        saveNumber = (i - 1) `mod` 10
        key = "game_state_" <> show saveNumber
      loadGame key
  where
    loadGame key = do
      { nextPlayerIndex } <- H.gets _.dominationConfig
      mbGame <- load key
      case mbGame of
        Left e -> error e
        Right activeState -> do
          let
            newActiveState = ActiveState.upgrade $
              (_playerIndex .~ nextPlayerIndex) activeState
          queryGame $ LoadActiveState newActiveState
          sendMessage $ GameMessage
            { state: activeState.state
            , i: activeState.i
            , playMade: Nothing
            }
    saveGame activeState = do
      let
        saveNumber = activeState.i `mod` 10
        key = "game_state_" <> show saveNumber
      save key activeState >>= logErrorToChat
      save "game_state" activeState >>= logErrorToChat
    queryGame state = do
      _ <- H.query
        Domination._component
        (AreaSlot GameArea)
        (state unit)
      pure unit
    sendChatMessage = do
      { id, message, chatNumber: oldChatNumber } <- H.get
      let
        chatNumber = oldChatNumber + 1
        chat = ChatMessage { username: id, message, chatNumber }
      H.modify_
        $ (_message .~ "") <<< (_messages :~ chat) <<< (_chatNumber .~ chatNumber)
      -- if we log errors to chat while sending messages: infinite loop?
      errorOrUnit <- saveChat
      case errorOrUnit of
        Left message -> error message
        Right _ -> pure unit
      sendMessage chat
    logErrorToChat result = case result of
      Left err -> do
        H.modify_ $ _message .~ ("ERROR: " <> err)
        sendChatMessage
      Right _ -> pure unit

sendMessage
  :: forall t1 r o1 q1 output m
  . Log m
  => Broadcast m
  => WireCodec m
  => RemoteMessage
  -> HalogenM AppState AppAction (ChildComponents t1 r o1 q1) output m Unit
sendMessage message' = do
  let message = view Message._toWire message'
  { roomCode, maybeBroadcaster, id } <- H.get
  log $ "maybeBroadcaster: " <> show maybeBroadcaster

  let wireEnvelope = Tuple id message

  announce <- H.gets _.announce

  maybeBroadcaster' <- case maybeBroadcaster of
    Nothing -> maybeCreateBroadcaster
      roomCode remoteMessageTarget localMessageTarget announce
    Just b -> pure (Just b)

  case maybeBroadcaster' of
    Nothing ->
      log "no broadcaster to send message"
    Just broadcaster -> do
      H.modify_ (_maybeBroadcaster .~ maybeBroadcaster')
      eString <- writeWire wireEnvelope
      case eString of
        Left e -> error e
        Right string ->
          broadcast broadcaster string
