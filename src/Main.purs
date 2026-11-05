--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Initialize a chat application with a game that can be played by multiple players using a WebSockets-based architecture, implementing features such as game state management, player management, and error handling.
--|
--| ### Key Concepts
--| * **Halogen**: A functional programming framework for building reactive user interfaces in Haskell.
--| * **State Management**: The module uses Halogen's state management system to manage the application's state and interact with its components.
--| * **Event Handling**: The module defines a `HandleGameEvent` function that handles game events, such as new states, saving games, and undoing moves.

module Main where

import Prelude

import AppAction (AppAction(..))
import AppState (AppState, CardSpecSelection, MailboxSession, SettingsTab(..), _botDelay, _mailbox, _mailboxUrl, defaultMailboxUrl, _chatNumber, _connectedClients, _connectionCount, _debugReport, _dominationConfig, _id, _inviteBaseUrl, _kingdom, _longGame, _maybeAudioContext, _maybeBroadcaster, _message, _relayReachable, _messages, _renameProposals, _room, _roomNotice, _serverUrl, _settingsTab, _showMenu, _username, _usernames, addBot, clampBotDelay, defaultKingdom, defaultServerUrl, newApp, normalizeRoster, removeBot, setPlayerCount, setPlayerIndex, upgradeSelection)
import DebugLog as DebugLog
import Audio.WebAudio.Types (AudioContext)
import Control.Monad.State (class MonadState)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Array (elem, length, take)
import Data.Foldable (traverse_)
import Data.Foldable as Foldable
import Data.Bifunctor (rmap)
import Data.Either (Either(..), either, hush)
import Data.HashMap as HashMap
import Data.Map as Map
import Effect.Aff.Class (class MonadAff, liftAff)
import Data.Lens.Getter (view)
import Data.Lens.Prism (review)
import Data.Lens.Setter (over, set, (%~), (.~))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Tuple (Tuple(..))
import Domination.AppM (runAppM)
import Domination.Capability.Audio (class Audio, newAudioContext, runAudioM)
import Domination.Capability.Broadcast (class Broadcast, broadcast, maybeCreateBroadcaster, reopen)
import Domination.Capability.Broadcast.WebSocket (WebSocketBroadcaster)
import Domination.Capability.Clock (class Clock, now)
import Domination.Capability.Dom (class Dom, locationHash, pageBaseUrl, reload, setLocationHash, window)
import Domination.Capability.GenUuid (class GenUuid, genUuid)
import Domination.Capability.Log (class Log, error, log)
import Domination.Capability.Mailbox (class Mailbox, isDocumentVisible, requestMailbox, wallClock)
import Domination.Capability.Random (class Random, randomElement, randomIntBetween, shuffle)
import Domination.Capability.Storage (class Storage, load, save)
import Domination.Capability.Timer (class Timer, createTimer)
import Domination.Capability.WireCodec (class WireCodec, readWire, writeWire)
import Domination.Data.Backoff as Backoff
import Domination.Data.Card (CardSpec)
import Domination.Data.Game (Game)
import Domination.Data.LogicalClock (Verdict(..), judge, savedBeforeVersions, senderVersion, tick, versionSender)
import Domination.Data.Mailbox (Delivery(..), Dirty(..), PullOutcome(..), Settlement(..), Stamp(..), Status(..), Sync, deliveryStamp, initialSync, interpretDelivery, interpretPull, isClean, mailboxBackoff, mailboxRoomUrl, overtakes, ownChange, printRevisionTag, pullCursor, receiveFrame, senderStamp, settleDelivery, settlePull, stampSender, status)
import Domination.Data.Resync (Occasion(..), Reaction(..), clockAt, hiddenFor, initialMarks, reactions)
import Domination.Data.Resync as Resync
import Domination.Data.Retheme (Proposals, proposeRename, renameMapping)
import Domination.Data.Room (Admission(..), Header(..), Room(..), admission, channelName, generateRoomCode, headerSender, isGameRoom, parseHeader, parseRoomHash, printHeader, roomFromHash, roomHash, scopedKey)
import Domination.Env (env)
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
import Effect.Aff as Aff
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
import Halogen.Query.Event (eventListener)
import Halogen.Query.HalogenM (HalogenM)
import Halogen.VDom.Driver (runUI)
import Message (RemoteMessage(..), WireEnvelope)
import Message as Message
import Web.Event.Event (EventType(..))
import Web.HTML.Event.BeforeUnloadEvent.EventTypes as ET
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document, toEventTarget)

remoteMessageTarget :: String
remoteMessageTarget = "remote-message-target"

uuidKey :: String
uuidKey = "player-id"

serverUrlKey :: String
serverUrlKey = "server-url"

usernameKey :: String
usernameKey = "username"

chatKey :: String
chatKey = "chat"

renameProposalsKey :: String
renameProposalsKey = "rename_proposals"

gameStateKey :: String
gameStateKey = "game_state"

mailboxUrlKey :: String
mailboxUrlKey = "mailbox-url"

mailboxSyncKey :: String
mailboxSyncKey = "mailbox_sync"

gameVersionKey :: String
gameVersionKey = "game_version"

mailboxTimeoutMs :: Int
mailboxTimeoutMs = 5000

mailboxNoticeSender :: String
mailboxNoticeSender = "📬"

caughtUpNotice :: String
caughtUpNotice = "(caught up: the game changed while you were away)"

notAppliedNotice :: String
notAppliedNotice = "(another move arrived first; yours was not applied)"

-- | The room's mailbox address, when async play is on: only in game rooms,
-- | and only with a mailbox URL.
mailboxTarget :: AppState -> Maybe String
mailboxTarget { room, mailboxUrl } = case room of
  GameRoom code | mailboxUrl /= "" -> Just $ mailboxRoomUrl mailboxUrl code
  _ -> Nothing

gameOf :: RemoteMessage -> Maybe Game
gameOf = case _ of
  GameMessage { state } -> Just state
  _ -> Nothing

undoKey :: Int -> String
undoKey saveNumber = "game_state_" <> show saveNumber

main :: Effect Unit
main = launchAff_ $ do
  liftEffect DebugLog.install
  liftEffect $ FFI.registerServiceWorker
  audioContext <- liftEffect $ runAudioM newAudioContext
  HA.awaitLoad
  body <- HA.awaitBody
  void $ runUI (root audioContext) unit body

root :: forall s query o. AudioContext -> Component query o s Aff
root audioContext = H.hoist (runAppM env) (component audioContext)

component
  :: forall m s query o
  . Storage m
  => Dom m
  => Log m
  => Random m
  => Clock m
  => GenUuid m
  => Broadcast WebSocketBroadcaster m
  => WireCodec m
  => Audio m
  => Timer m
  => Mailbox m
  => MonadAff m
  => AudioContext
  -> Component query o s m
component audioContext =
  H.mkComponent { eval, initialState, render: render audioContext }
  where
  eval = H.mkEval H.defaultEval
    { handleAction = handleAction audioContext
    , initialize = Just Initialize
    , finalize = Just Finalize
    }
  initialState _ = newApp

render
  :: forall c m
  . Log m
  => Audio m
  => Storage m
  => Dom m
  => Random m
  => MonadAff m
  => AudioContext
  -> AppState
  -> HTML (Domination.Component GameQuery c m AppAction) AppAction
render audioContext state = HH.main_ $
  [ HH.div
    [ HP.id $ remoteMessageTarget
    , HE.handler (EventType "purescript") ReceiveRemoteMessage
    , HE.handler (EventType "disconnected") (const RelayLost)
    , HE.handler (EventType "reconnected") (const RelayRegained)
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
  , renderMailboxStatus state
  , renderConnections state
  ]

renderMailboxStatus :: forall w i. AppState -> HTML w i
renderMailboxStatus state = maybe (HH.text "") (const mark) (mailboxTarget state)
  where
    mark = case status { sync: state.mailbox.sync, unreachable: state.mailbox.unreachable } of
      Synced -> render' [] "☁ ✓"
        "Saved in the room mailbox: players who are away will get this game"
      Pending -> render' [] "☁ …" "Saving the game to the room mailbox"
      Offline -> render' [ Css.offline ] "☁ ✗"
        "The room mailbox is unreachable; retrying"
    render' classes text title = HH.i
      [ HP.classes $ [ Css.mailboxStatus ] <> classes, HP.title title ]
      [ HH.text text ]

renderConnections :: forall w i. AppState -> HTML w i
renderConnections { relayReachable, connectionCount }
  | relayReachable = HH.i
      [ HP.class_ Css.connections ]
      [ HH.text $ show connectionCount ]
  | otherwise = HH.i
      [ HP.classes [ Css.connections, Css.offline ]
      , HP.title "Offline: reconnecting to the server"
      ]
      [ HH.text "…" ]

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
  => MonadState { messages :: messages, room :: Room | r } m
  => Storage m
  => EncodeJson messages
  => DecodeJson messages
  => Log m
  => m (Either String Unit)
saveChat = do
  { messages, room } <- H.get
  result <- save (scopedKey room chatKey) messages
  log "saved chat messages"
  pure result


handleAction
  :: forall output m t1 r o1 q1
  . Audio m
  => Storage m
  => Log m
  => GenUuid m
  => Random m
  => Clock m
  => Broadcast WebSocketBroadcaster m
  => WireCodec m
  => Timer m
  => Dom m
  => Mailbox m
  => MonadAff m
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

    eRoom <- parseRoomHash <$> locationHash
    inviteBaseUrl <- pageBaseUrl
    let room = either (const GlobalRoom) identity eRoom
    either (log <<< append "Initialize: ignoring invalid room link: ") (const $ pure unit) eRoom
    log $ "Initialize: room: " <> show room
    H.modify_ $ (_room .~ room)
      >>> (_roomNotice .~ either Just (const Nothing) eRoom)
      >>> (_inviteBaseUrl .~ inviteBaseUrl)

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

    eBotStrategies <- load "bot_strategies"
    botStrategies <- case eBotStrategies of
      Left e -> do
        log $ "Initialize: Failed to load botStrategies. Falling back to default."
          <> "Error: " <> e
        pure []
      Right s -> pure s

    eBotDelay <- load "bot_delay"
    botDelay <- case eBotDelay of
      Left e -> do
        log $ "Initialize: Failed to load botDelay. Falling back to default."
          <> "Error: " <> e
        pure 2000
      Right d -> pure $ clampBotDelay d

    eRenameProposals <- load renameProposalsKey
    renameProposals <- case eRenameProposals of
      Left e -> do
        log $ "Initialize: no rename proposals found. Error: " <> e
        pure Map.empty
      Right p -> pure p

    eMessages <- load (scopedKey room chatKey)
    messages <- case eMessages of
      Left e -> do
        log $ "Initialize: Failed to load messages. Error: " <> e
        pure []
      Right m -> pure $ Message.retainRecentMessages m

    log "Initialize: modify a bunch of stuff..."
    H.modify_ $ (_id .~ uuid)
      >>> (_username .~ username)
      >>> (_usernames %~ HashMap.insert uuid username)
      >>> (_dominationConfig <<< _kingdom .~ kingdom)
      >>> (_dominationConfig %~ \config -> normalizeRoster config
        { nextPlayerIndex = nextPlayerIndex
        , nextPlayerCount = nextPlayerCount
        , botStrategies = botStrategies
        })
      >>> (_dominationConfig <<< _botDelay .~ botDelay)
      >>> (_maybeAudioContext .~ Just audioContext)
      >>> (_messages .~ messages)
      >>> (_renameProposals .~ renameProposals)
    log "Initialize: done modifying a bunch of stuff..."

    eMailboxUrl <- load mailboxUrlKey
    mailboxUrl <- either
      (const $ pure defaultMailboxUrl)
      pure
      eMailboxUrl
    eSync <- load (scopedKey room mailboxSyncKey)
    let (sync :: Sync) = either (const initialSync) identity eSync
    H.modify_ $ (_mailboxUrl .~ mailboxUrl) >>> (_mailbox %~ _ { sync = sync })

    log "Initialize: load game..."
    restored <- restoreGame gameStateKey
    eVersion <- load (scopedKey room gameVersionKey)
    let version = either (const $ savedBeforeVersions <<< _.i <$> restored) Just eVersion
    H.modify_ $ _mailbox %~ _ { outbox = toGameMessage <$> restored, version = version }
    when (isGameRoom room && isNothing restored) $ H.modify_
      $ (_showMenu .~ true) >>> (_settingsTab .~ GameSetupTab)
    configureBots
    log "Initialize: done loading game..."

    eServerUrl <- load serverUrlKey
    serverUrl <- case eServerUrl of
      Left e -> do
        log $ "Initialize: no existing server URL found, using default."
          <> " error: " <> e
        pure $ defaultServerUrl
      Right u -> pure u

    log $ "Initialize: server URL: " <> serverUrl
    H.modify_ (_serverUrl .~ serverUrl)

    startedAt <- wallClock
    H.modify_ _ { resync = initialMarks startedAt }

    maybeBroadcaster <- maybeCreateBroadcaster
      (channelName room) remoteMessageTarget serverUrl

    log $ "Initialize: broadcaster: " <> show maybeBroadcaster

    H.modify_ (_maybeBroadcaster .~ maybeBroadcaster)

    broadcaster <- H.gets _.maybeBroadcaster

    log $ "Initialize: broadcaster: " <> show broadcaster

    interval <- H.gets _.heartbeatInterval
    _ <- H.subscribe =<< createTimer { interval } HeartbeatTick

    w <- window
    H.subscribe' \_ ->
      eventListener
        ET.beforeunload
        (toEventTarget w)
        \_ -> Just Finalize

    H.subscribe' \_ ->
      eventListener
        (EventType "unload")
        (toEventTarget w)
        \_ -> Just Finalize

    H.subscribe' \_ ->
      eventListener
        (EventType "hashchange")
        (toEventTarget w)
        \_ -> Just RoomChanged

    H.subscribe' \_ ->
      eventListener
        (EventType "online")
        (toEventTarget w)
        \_ -> Just NetworkRegained

    doc <- liftEffect $ document w
    H.subscribe' \_ ->
      eventListener
        (EventType "visibilitychange")
        (HTMLDocument.toEventTarget doc)
        \_ -> Just VisibilityChanged

    log "Initialize completed"
    resync Started

  Finalize -> do
    clientId <- H.gets _.id
    timestamp <- now
    sendMessage $ LeaveMessage { clientId, timestamp }
    pure unit

  ToggleMenu -> do
    H.modify_ $ _showMenu %~ not
    void refreshDebugReport

  WritePlayerIndex index -> updateRoster $ setPlayerIndex index

  WritePlayerCount count -> updateRoster $ setPlayerCount count

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

  AddBot strategy -> do
    updateRoster $ addBot strategy
    configureBots

  RemoveBot botIndex -> do
    updateRoster $ removeBot botIndex
    configureBots

  DoNothing -> pure unit

  WriteBotDelay delay -> do
    let botDelay = clampBotDelay delay
    H.modify_ $ _dominationConfig <<< _botDelay .~ botDelay
    save "bot_delay" botDelay >>= logErrorToChat
    configureBots

  SwitchSettingsTab tab -> do
    H.modify_ $ _settingsTab .~ tab
    void refreshDebugReport

  CopyBugReport -> do
    report <- refreshDebugReport
    liftEffect $ FFI.copyToClipboard report

  ClearDebugLog -> do
    liftEffect DebugLog.clear
    void refreshDebugReport

  CreateRoom -> generateRoomCode >>= maybe
    (error "CreateRoom: failed to generate a room code")
    (setLocationHash <<< roomHash <<< GameRoom)

  LeaveRoom -> setLocationHash $ roomHash GlobalRoom

  RoomChanged -> do
    current <- H.gets _.room
    next <- roomFromHash <$> locationHash
    log $ "RoomChanged: " <> show current <> " -> " <> show next
    when (next /= current) reload

  CopyInviteLink -> do
    { inviteBaseUrl, room } <- H.get
    liftEffect $ FFI.copyToClipboard $ inviteBaseUrl <> roomHash room

  ProposeRename target text -> do
    { renameProposals } <- H.modify $ _renameProposals %~ proposeRename target text
    save renameProposalsKey renameProposals >>= logErrorToChat

  CopyRenameMapping -> do
    proposals <- H.gets _.renameProposals
    liftEffect $ FFI.copyToClipboard $ renameMapping proposals

  ClearRenameProposals -> do
    H.modify_ $ _renameProposals .~ Map.empty
    save renameProposalsKey (Map.empty :: Proposals) >>= logErrorToChat

  StartNewGame -> do
    config <- H.gets _.dominationConfig
    queryGame $ StartNewGameRequest config
    H.modify_ $ _showMenu .~ false

  LoadGameRequest -> do
    loadGame gameStateKey
    H.modify_ $ _showMenu .~ false

  WriteServerUrl serverUrl -> do
    save serverUrlKey serverUrl >>= logErrorToChat
    log $ "saving server URL: " <> serverUrl
    H.modify_ $ set _serverUrl serverUrl

  WriteMailboxUrl mailboxUrl -> do
    save mailboxUrlKey mailboxUrl >>= logErrorToChat
    H.modify_ $ _mailboxUrl .~ mailboxUrl

  DeliverMailbox -> deliverMailbox

  MailboxRetryDue -> do
    modifySession _ { retryScheduled = false }
    resync RetryDue

  VisibilityChanged -> do
    visible <- isDocumentVisible
    at <- wallClock
    marks <- H.gets _.resync
    if visible
    then do
      H.modify_ _ { resync = Resync.shown marks }
      resync $ Shown $ hiddenFor at marks
    else H.modify_ _ { resync = Resync.hidden at marks }

  NetworkRegained -> resync NetworkBack

  WriteUsername username -> do
    { id } <- H.get
    save usernameKey username >>= logErrorToChat
    H.modify_ $ set _username username
      <<< over _usernames (HashMap.insert id username)
    sendMessage $ UsernameMessage { username, id }

  Write lens value -> H.modify_ $ set lens value
  SendMessage -> sendChatMessage

  ReceiveRemoteMessage customEvent -> do
    heardAt <- wallClock
    H.modify_ \st -> st { resync = Resync.heard heardAt st.resync }
    let detail = FFI.detail customEvent
    room <- H.gets _.room
    (eHeader :: Either String String) <- readWire detail
    case admission room <$> eHeader of
      Left e -> error $ "problem receiving message header: " <> e
      Right (ForeignRoom senderRoom) ->
        log $ "dropped message for " <> show senderRoom
      Right (Unsupported headerError) ->
        log $ "dropped message with unsupported header: " <> show headerError
      Right Admit -> do
        (eWireEnvelope :: Either String WireEnvelope) <- readWire detail
        case rmap (review Message._toWire) <$> eWireEnvelope of
          Left e -> error $ "problem receiving message: " <> e
          Right (Tuple header msg) -> receiveMessage header msg
  HandleGameEvent gameEvent -> case gameEvent of
    NewState activeState playMade -> do
      case playMade of
        Just x -> H.modify_ $ _messages %~ Message.rememberMessage (PlayMadeMessage x)
        Nothing -> pure unit
      queryGame $ LoadActiveState activeState
      saveGame activeState
      publishGame $ GameMessage
        { state: activeState.state
        , i: activeState.i
        , playMade
        }
    SaveGame activeState -> saveGame activeState
    Undo { i } -> loadGame $ undoKey $ (i - 1) `mod` 10
  HeartbeatTick -> do
    { id: clientId, relayReachable, resync: marks } <- H.get
    timestamp <- now
    when relayReachable $
      sendMessage $ HeartbeatMessage { clientId, timestamp }
    visible <- isDocumentVisible
    at <- wallClock
    resync $ Ticked $ clockAt at { visible, relayOpen: relayReachable } marks
  RelayLost -> do
    log "RelayLost: queueing messages until the server is back"
    H.modify_ $ _relayReachable .~ false
  RelayRegained -> do
    log "RelayRegained: announcing ourselves again"
    H.modify_ $ _relayReachable .~ true
    resync RelayBack
  where
    -- When to look again for the room's latest game: Domination.Data.Resync.
    resync occasion = traverse_ react (reactions occasion)
    react = case _ of
      Announce -> do
        { id: clientId, username } <- H.get
        timestamp <- now
        sendMessage $ JoinMessage { clientId, timestamp }
        sendMessage $ UsernameMessage { username, id: clientId }
      OfferGame -> offerGame
      PullMailbox -> pullMailbox
      ReopenRelay -> do
        at <- wallClock
        H.modify_ \st -> st { resync = Resync.reopened at st.resync }
        log "Resync: reopening the relay socket"
        H.gets _.maybeBroadcaster >>= traverse_ reopen
    receiveMessage header msg = case msg of
        UsernameMessage { username, id } -> do
          log $ "username incoming: " <> username
          H.modify_ $ over _usernames
            $ HashMap.insert id username
        ChatMessage { message } -> do
          H.modify_ $ _messages %~ Message.rememberMessage msg
          saveChat >>= logErrorToChat
          if message == "PING"
            then do
              H.modify_ $ _message .~ "PONG"
              sendChatMessage
            else pure unit
        GameMessage _ -> receiveGameFrame header msg
        PlayMadeMessage _ ->
          error "PlayMadeMessage should not be called"
        JoinMessage { clientId } -> do
          timestamp <- now
          let clientInfo = { timestamp, clientId }
          H.modify_ $ _connectedClients %~ HashMap.insert clientId clientInfo
          -- After updating connected clients, broadcast the new count
          clients <- H.gets _.connectedClients
          let count = HashMap.size clients
          H.modify_ $ _connectionCount .~ count
          log $ "Client joined: " <> clientId <> ", total clients: " <> show count
          resync PeerJoined
        LeaveMessage { clientId } -> do
          H.modify_ $ _connectedClients %~ HashMap.delete clientId
          -- After removing client, broadcast the new count
          clients <- H.gets _.connectedClients
          let count = HashMap.size clients
          H.modify_ $ _connectionCount .~ count
          log $ "Client left: " <> clientId <> ", total clients: " <> show count
        HeartbeatMessage { clientId } -> do
          timestamp <- now
          -- First get current state
          clients <- H.gets _.connectedClients
          let originalClientCount = HashMap.size clients
          timeout <- H.gets _.heartbeatTimeout

          -- Clean up stale clients first
          let activeClients = HashMap.filter (\info -> not $ (timestamp - info.timestamp) > timeout) clients
          let activeClientCount = HashMap.size activeClients

          -- Update the current client's heartbeat
          let updatedClients = HashMap.insert
                clientId
                { timestamp, clientId }
                activeClients
          let updatedClientCount = HashMap.size updatedClients

          -- Update state with clean list including current client
          H.modify_ $ _connectedClients .~ updatedClients
          H.modify_ $ _connectionCount .~ updatedClientCount

          { username, id } <- H.get
          sendMessage (UsernameMessage { username, id })

          log $ "Heartbeat from clientId(" <> clientId <> ")"
            <> "; Clients before cleanup: " <> show originalClientCount
            <> "; Clients after cleanup: " <> show activeClientCount
            <> "; Clients after new heartbeat: " <> show updatedClientCount
    loadGame key = restoreGame key >>= traverse_ shareGame
    restoreGame key = do
      { dominationConfig: { nextPlayerIndex }, room } <- H.get
      mbGame <- load $ scopedKey room key
      case mbGame of
        Left e -> do
          error e
          pure Nothing
        Right activeState -> do
          let
            newActiveState = ActiveState.upgrade
              $ (_playerIndex .~ nextPlayerIndex)
              $ activeState
          queryGame $ LoadActiveState newActiveState
          pure $ Just activeState
    shareGame = publishGame <<< toGameMessage
    saveGame activeState = do
      room <- H.gets _.room
      save (scopedKey room $ undoKey $ activeState.i `mod` 10) activeState >>= logErrorToChat
      save (scopedKey room gameStateKey) activeState >>= logErrorToChat
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
        $ (_message .~ "")
        <<< (_messages %~ Message.rememberMessage chat)
        <<< (_chatNumber .~ chatNumber)
      -- if we log errors to chat while sending messages: infinite loop?
      errorOrUnit <- saveChat
      case errorOrUnit of
        Left errorMessage -> error errorMessage
        Right _ -> pure unit
      sendMessage chat
    logErrorToChat result = case result of
      Left err -> do
        H.modify_ $ _message .~ ("ERROR: " <> err)
        sendChatMessage
      Right _ -> pure unit
    updateRoster changeRoster = do
      H.modify_ $ _dominationConfig %~ changeRoster
      { nextPlayerIndex, nextPlayerCount, botStrategies } <- H.gets _.dominationConfig
      save "player_index" nextPlayerIndex >>= logErrorToChat
      save "player_count" nextPlayerCount >>= logErrorToChat
      save "bot_strategies" botStrategies >>= logErrorToChat
    configureBots = do
      { botStrategies, botDelay } <- H.gets _.dominationConfig
      queryGame $ ConfigureBots { botStrategies, botDelay }
    -- Async play: the room mailbox (design §6.4). The decisions live in
    -- Domination.Data.Mailbox; these helpers perform them.
    modifySession :: (MailboxSession -> MailboxSession) -> _
    modifySession f = H.modify_ $ _mailbox %~ f
    setSync sync = do
      modifySession _ { sync = sync }
      room <- H.gets _.room
      save (scopedKey room mailboxSyncKey) sync >>= logErrorToChat
    markReachable = modifySession _ { failures = 0, unreachable = false }
    notify message = do
      H.modify_ $ _messages %~ Message.rememberMessage
        (ChatMessage { username: mailboxNoticeSender, message, chatNumber: 0 })
      saveChat >>= logErrorToChat
    applyGame msg = case msg of
      GameMessage { i, state, playMade } -> do
        firstGame <- H.gets \st -> isGameRoom st.room && isNothing st.mailbox.outbox
        -- A room opened without a saved game shows game setup, and the game
        -- view still has its default seat; the first game to arrive takes
        -- the seat chosen in setup, as a restored game does.
        when firstGame do
          { nextPlayerIndex } <- H.gets _.dominationConfig
          queryGame $ LoadActiveState
            { i, state, playerIndex: nextPlayerIndex
            , playerCount: Foldable.length state.players, showSupply: true
            }
          H.modify_ $ _showMenu .~ false
        queryGame $ ReceiveGame { i, state }
        traverse_ (\x -> H.modify_ $ _messages %~ Message.rememberMessage (PlayMadeMessage x)) playMade
        modifySession \session -> session
          { outbox = Just msg, adoptions = session.adoptions + 1 }
      _ -> pure unit
    differsFromOurs msg = do
      outbox <- H.gets _.mailbox.outbox
      pure $ (outbox >>= gameOf) /= gameOf msg
    -- Own changes go to the mailbox first and are broadcast once committed.
    -- Without a mailbox they are broadcast at once, versioned.
    publishGame msg = do
      enabled <- H.gets $ isJust <<< mailboxTarget
      modifySession \session -> session
        { outbox = Just msg, moves = session.moves + 1 }
      if enabled
      then do
        setSync <<< ownChange =<< H.gets _.mailbox.sync
        deliverMailbox
      else do
        { id, mailbox: { version } } <- H.get
        setVersion $ tick id version
        offerGame
    setVersion version = do
      modifySession _ { version = Just version }
      room <- H.gets _.room
      save (scopedKey room gameVersionKey) version >>= logErrorToChat
    -- Without a mailbox, the newest version wins (Domination.Data.LogicalClock):
    -- whoever joins and whoever sees them join offer their games, and each
    -- side takes only a game ahead of its own.
    offerGame = do
      enabled <- H.gets $ isJust <<< mailboxTarget
      { outbox, version } <- H.gets _.mailbox
      unless enabled $ traverse_ identity
        $ broadcastVersioned <$> version <*> outbox
    broadcastVersioned version msg = encodeEnvelope (versionSender version) msg
      >>= either error broadcastWire
    receiveGameFrame header msg = do
      enabled <- H.gets $ isJust <<< mailboxTarget
      previous <- H.gets _.mailbox.sync
      let sender = either (const Nothing) (Just <<< headerSender) (parseHeader header)
      let stamp = senderStamp =<< sender
      if enabled then receiveStamped header msg previous stamp
      else do
        { id, mailbox: { version } } <- H.get
        case judge id version (senderVersion =<< sender) of
          TakeTheirs next -> do
            applyGame msg
            setVersion next
          SendOurs -> do
            log $ "Clock: answered a game behind ours " <> header
            offerGame
          AlreadyHave -> log $ "Clock: ignored the game we already have " <> header
    receiveStamped header msg previous stamp =
      case receiveFrame stamp previous of
        Nothing -> log $ "Mailbox: ignored an overtaken game frame " <> header
        Just next -> do
          differs <- differsFromOurs msg
          applyGame msg
          setSync next
          when (differs && overtakes previous next) $ notify notAppliedNotice
          unless (isClean next) deliverMailbox
    adoptEnvelope envelope = do
      room <- H.gets _.room
      (eHeader :: Either String String) <- readWire envelope
      case admission room <$> eHeader of
        Right Admit -> do
          (eWireEnvelope :: Either String WireEnvelope) <- readWire envelope
          case rmap (review Message._toWire) <$> eWireEnvelope of
            Right (Tuple _ msg@(GameMessage _)) -> do
              applyGame msg
              pure $ Just msg
            Right _ -> do
              error "Mailbox: the room mailbox held something other than a game"
              pure Nothing
            Left e -> do
              error $ "Mailbox: could not decode the stored game: " <> e
              pure Nothing
        other -> do
          error $ "Mailbox: stored game not admitted: " <> show other
          pure Nothing
    broadcastGame stamp msg = encodeEnvelope (stampSender stamp) msg
      >>= either error broadcastWire
    pullMailbox = do
      pulledAt <- wallClock
      H.modify_ \st -> st { resync = Resync.pulled pulledAt st.resync }
      state <- H.get
      let session = state.mailbox
      case mailboxTarget state of
        Nothing -> pure unit
        Just url
          | not (isClean session.sync) -> deliverMailbox
          | session.inFlight -> pure unit
          | otherwise -> do
              modifySession _ { inFlight = true }
              response <- requestMailbox
                { method: "GET"
                , url
                , ifMatch: Nothing
                , ifNoneMatch: pullCursor session.sync
                , body: Nothing
                , timeoutMs: mailboxTimeoutMs
                }
              modifySession _ { inFlight = false }
              after <- H.gets _.mailbox
              let pulled = interpretPull response
              log $ "Mailbox: GET " <> url <> " -> " <> show pulled
              if not (isClean after.sync)
              then deliverMailbox
              else case settlePull (isJust after.outbox) after.sync pulled of
                UpToDate -> markReachable
                CatchUp { sync, envelope } -> do
                  markReachable
                  adopted <- adoptEnvelope envelope
                  when (isJust adopted) do
                    setSync sync
                    notify caughtUpNotice
                Upload sync -> do
                  markReachable
                  setSync sync
                  deliverMailbox
                PullRetry e -> do
                  log $ "Mailbox: " <> e
                  modifySession _ { unreachable = true }
                  scheduleRetry
    deliverMailbox = do
      state <- H.get
      let session = state.mailbox
      case mailboxTarget state, session.outbox of
        Just url, Just outbox
          | not session.inFlight && not (isClean session.sync) -> do
              eEnvelope <- encodeEnvelope (stampSender $ deliveryStamp session.sync) outbox
              case eEnvelope of
                Left e -> error e
                Right envelope -> do
                  let
                    -- While the mailbox is known to be down, live play does
                    -- not wait for it.
                    broadcastFirst = session.unreachable && session.sync.dirty == Moved
                    sent =
                      { base: session.sync.confirmed
                      , envelope
                      , moves: session.moves
                      , adoptions: session.adoptions
                      }
                  when broadcastFirst $ broadcastGame (Tentative sent.base) outbox
                  modifySession _ { inFlight = true }
                  response <- requestMailbox
                    { method: "PUT"
                    , url
                    , ifMatch: Just $ printRevisionTag sent.base
                    , ifNoneMatch: Nothing
                    , body: Just envelope
                    , timeoutMs: mailboxTimeoutMs
                    }
                  modifySession _ { inFlight = false }
                  after <- H.gets _.mailbox
                  let delivery = interpretDelivery response
                  log $ "Mailbox: PUT " <> url <> " -> " <> show delivery
                  mine <- case delivery, after.outbox of
                    Conflict r _, Just game ->
                      hush <$> encodeEnvelope (stampSender $ Committed r) game
                    _, _ -> pure Nothing
                  settleDelivered broadcastFirst
                    $ settleDelivery sent
                      { moves: after.moves, adoptions: after.adoptions }
                      mine
                      after.sync
                      delivery
        _, _ -> pure unit
    settleDelivered alreadyBroadcast = case _ of
      Stale -> do
        markReachable
        continueDelivery
      Accepted { sync, broadcast: envelope, replay, notApplied } -> do
        markReachable
        when replay $ void $ adoptEnvelope envelope
        setSync sync
        broadcastWire envelope
        when notApplied $ notify notAppliedNotice
        continueDelivery
      Adopt { sync, envelope, notApplied } -> do
        markReachable
        previous <- H.gets _.mailbox.outbox
        adopted <- adoptEnvelope envelope
        setSync sync
        when (notApplied && (previous >>= gameOf) /= (adopted >>= gameOf))
          $ notify notAppliedNotice
        continueDelivery
      Redeliver sync -> do
        markReachable
        setSync sync
        continueDelivery
      Retry { sync, broadcastTentative } -> do
        setSync sync
        modifySession _ { unreachable = true }
        when (broadcastTentative && not alreadyBroadcast)
          $ traverse_ (broadcastGame $ Tentative sync.confirmed)
          =<< H.gets _.mailbox.outbox
        scheduleRetry
    continueDelivery = do
      clean <- H.gets $ isClean <<< _.mailbox.sync
      unless clean deliverMailbox
    scheduleRetry = do
      session <- H.gets _.mailbox
      unless session.retryScheduled do
        let failures = session.failures + 1
        modifySession _ { retryScheduled = true, failures = failures }
        sample <- (_ / 1000.0) <<< toNumber <$> randomIntBetween 0 1000
        let wait = Backoff.delay mailboxBackoff failures sample
        log $ "Mailbox: unreachable; retrying in " <> show wait
        void $ H.fork do
          liftAff $ Aff.delay wait
          handleAction audioContext MailboxRetryDue
    refreshDebugReport = do
      report <- liftEffect $ DebugLog.renderReport <$> DebugLog.currentReport
      H.modify_ $ _debugReport .~ report
      pure report

sendMessage
  :: forall t1 r o1 q1 output m
  . Log m
  => Broadcast WebSocketBroadcaster m
  => WireCodec m
  => RemoteMessage
  -> HalogenM AppState AppAction (ChildComponents t1 r o1 q1) output m Unit
sendMessage message = encodeEnvelope identity message >>= either error broadcastWire

toGameMessage :: forall r. { state :: Game, i :: Int | r } -> RemoteMessage
toGameMessage { state, i } = GameMessage { state, i, playMade: Nothing }

-- | Game envelopes carry a mailbox stamp or a version after the sender id
-- | (§6.4.3); `decorate` adds it.
encodeEnvelope
  :: forall t1 r o1 q1 output m
  . WireCodec m
  => (String -> String)
  -> RemoteMessage
  -> HalogenM AppState AppAction (ChildComponents t1 r o1 q1) output m (Either String String)
encodeEnvelope decorate message = do
  { room, id } <- H.get
  let sender = decorate id
  writeWire $ Tuple (printHeader $ V1Header room sender) (view Message._toWire message)

broadcastWire
  :: forall t1 r o1 q1 output m
  . Log m
  => Broadcast WebSocketBroadcaster m
  => String
  -> HalogenM AppState AppAction (ChildComponents t1 r o1 q1) output m Unit
broadcastWire string = do
  { room, maybeBroadcaster } <- H.get
  log $ "maybeBroadcaster: " <> show maybeBroadcaster

  serverUrl <- H.gets _.serverUrl

  maybeBroadcaster' <- case maybeBroadcaster of
    Nothing -> maybeCreateBroadcaster
      (channelName room) remoteMessageTarget serverUrl
    Just b -> pure (Just b)

  case maybeBroadcaster' of
    Nothing ->
      log "no broadcaster to send message"
    Just broadcaster -> do
      H.modify_ (_maybeBroadcaster .~ maybeBroadcaster')
      broadcast broadcaster string
