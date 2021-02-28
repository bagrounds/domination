module Main where

import Prelude

import AppAction (AppAction(..))
import AppState (AppState, _connectionCount, _dominationConfig, _id, _kingdom, _maybeBroadcaster, _message, _messages, _nextPlayerCount, _nextPlayerIndex, _showMenu, _username, _usernames, defaultKingdom, newApp, upgradeSelection)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.HashMap as HashMap
import Data.Lens.Getter (view)
import Data.Lens.Prism (review)
import Data.Lens.Setter (over, set, (%~), (.~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Domination.AppM (runAppM)
import Domination.Capability.Audio (class Audio)
import Domination.Capability.Broadcast (class Broadcast, broadcast, create)
import Domination.Capability.Dom (class Dom)
import Domination.Capability.GenUuid (class GenUuid, genUuid)
import Domination.Capability.Log (class Log, error, log)
import Domination.Capability.Random (class Random, randomElement)
import Domination.Capability.Storage (class Storage, load, save)
import Domination.Capability.WireCodec (class WireCodec, readWire, writeWire)
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
  => Audio m
  => Component HTML query o s m
component = H.mkComponent { eval, initialState, render } where
  eval = H.mkEval H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
  initialState _ = newApp

--render
--  :: forall c m
--  . Log m
--  => Storage m
--  => Dom m
--  => Random m
--  => AppState
--  -> HTML (Domination.Component GameQuery c m AppAction) AppAction
render state = HH.main_ $
  [ HH.div
    [ HP.id_ $ remoteMessageTarget
    , HE.handler (EventType "purescript") (Just <<< ReceiveRemoteMessage)
    ]
    []
  , HH.div
    [ HP.id_ $ localMessageTarget
    , HE.handler
      (EventType "purescript")
      (Just <<< ReceiveLocalMessage)
    ]
    []
  , Settings.render state
  , renderSettingsButton
  , HH.slot
      Domination._component
      (AreaSlot GameArea)
      (Domination.component state.dominationConfig)
      unit
      (Just <<< HandleGameEvent)
  , Chat.render
    { sendEvent: SendMessage
    , onInput: Write _message
    , state
    }
  , HH.i
    [ HP.class_ Css.connections ]
    [ HH.text $ show state.connectionCount ]
  ]

renderSettingsButton :: forall w. HTML w AppAction
renderSettingsButton = HH.button
  [ HP.class_ Css.settingsButton
  , HE.onClick \_ -> Just ToggleMenu
  ]
  [ Icons.settings ]

type ChildComponents o r q1 o1 =
  ( "Domination" :: H.Slot GameQuery o DomSlot
  , "description" :: H.Slot q1 o1 DomSlot
  | r
  )
handleAction
  :: forall output m t1 r o1 q1
  . Storage m
  => Log m
  => GenUuid m
  => Random m
  => Broadcast m
  => WireCodec m
  => AppAction -> HalogenM AppState AppAction (ChildComponents t1 r o1 q1) output m Unit
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

    roomCode <- H.gets _.roomCode

    broadcaster <- create
      roomCode remoteMessageTarget localMessageTarget

    eKingdom <- load "kingdom"
    kingdom <- case eKingdom of
      Left e -> do
        error $ "Failed to load kingdom. Falling back to default."
          <> "Error: " <> e
        pure defaultKingdom
      Right k -> pure $ upgradeSelection <$> k

    ePlayerIndex <- load "player_index"
    nextPlayerIndex <- case ePlayerIndex of
      Left e -> do
        error $ "Failed to load playerIndex. Falling back to default."
          <> "Error: " <> e
        pure 0
      Right i -> pure i

    ePlayerCount <- load "player_count"
    nextPlayerCount <- case ePlayerCount of
      Left e -> do
        error $ "Failed to load playerCount. Falling back to default."
          <> "Error: " <> e
        pure 1
      Right i -> pure i

    H.modify_ $ (_id .~ uuid)
      >>> (_username .~ username)
      >>> (_usernames %~ HashMap.insert uuid username)
      >>> (_maybeBroadcaster .~ Just broadcaster)
      >>> (_dominationConfig <<< _kingdom .~ kingdom)
      >>> (_dominationConfig <<< _nextPlayerIndex .~ nextPlayerIndex)
      >>> (_dominationConfig <<< _nextPlayerCount .~ nextPlayerCount)

    loadGame "game_state"

  ToggleMenu -> H.modify_ $ _showMenu %~ not

  WritePlayerIndex index -> do
    { dominationConfig: { nextPlayerIndex, nextPlayerCount } } <- H.get
    log $ "Domination: updating playerIndex from "
      <> show nextPlayerIndex <> " -> " <> show index
    let
      newPlayerIndex = max index zero
      newPlayerCount = max (index + one) nextPlayerCount
    H.modify_ $ (_dominationConfig <<< _nextPlayerIndex .~ newPlayerIndex)
      >>> (_dominationConfig <<< _nextPlayerCount .~ newPlayerCount)
    log $ "saving player_index: " <> show newPlayerIndex
    save "player_index" newPlayerIndex
    log $ "saving player_count: " <> show newPlayerCount
    save "player_count" newPlayerCount

  WritePlayerCount count -> do
    { nextPlayerIndex, nextPlayerCount } <- H.gets _.dominationConfig
    log $ "Domination: updating playerCount from "
      <> show nextPlayerCount <> " -> " <> show count
    let
      newPlayerIndex = min (count - one) nextPlayerIndex
      newPlayerCount = max count one
    H.modify_
      $ (_dominationConfig <<< _nextPlayerCount .~ newPlayerCount)
      >>> (_dominationConfig <<< _nextPlayerIndex .~ newPlayerIndex)
    log $ "saving player_index: " <> show newPlayerIndex
    save "player_index" newPlayerIndex
    log $ "saving player_count: " <> show newPlayerCount
    save "player_count" newPlayerCount

  ChooseKingdom kingdom -> do
    save "kingdom" kingdom
    H.modify_ $ _dominationConfig <<< _kingdom .~ kingdom

  StartNewGame -> do
    config <- H.gets _.dominationConfig
    queryGame $ StartNewGameRequest config
    H.modify_ $ _showMenu .~ false

  LoadGameRequest -> do
    loadGame "game_state"
    H.modify_ $ _showMenu .~ false

  WriteUsername username -> do
    { id, usernames } <- H.get
    save "username" username
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
    log $ "ReceiveRemoteMessage: " <> detail
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
            log $ "username incoming: "
              <> username
            H.modify_ $ over _usernames
              $ HashMap.insert id username
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
      case playMade of
        Just x -> do
          log $ "Main: PlayMade"
          H.modify_ $ _messages :~ (PlayMadeMessage x)
        Nothing -> pure unit
      log $ "saving state as player" <> show activeState.playerIndex
      queryGame $ LoadActiveState activeState
      sendMessage $ GameStateMessage
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
      log $ "Main: LoadGame"
      mbGameState <- load key
      case mbGameState of
        Left e -> error e
        Right activeState -> do
          log $ "Main: LoadGame successful as player"
            <> show nextPlayerIndex
          let
            newActiveState = ActiveState.upgrade $
              (_playerIndex .~ nextPlayerIndex) activeState
          queryGame $ LoadActiveState newActiveState
          sendMessage $ GameStateMessage
            { state: activeState.state
            , i: activeState.i
            , playMade: Nothing
            }
    saveGame activeState = do
      log $ "saving state as player"
        <> show activeState.playerIndex
      let
        saveNumber = activeState.i `mod` 10
        key = "game_state_" <> show saveNumber
      save key activeState
      save "game_state" activeState
    queryGame state = do
      _ <- H.query
        Domination._component
        (AreaSlot GameArea)
        (state unit)
      pure unit
    sendChatMessage = do
      { id, message } <- H.get
      let chat = ChatMessage { username: id, message }
      H.modify_ $ (_message .~ "") <<< (_messages :~ chat)
      sendMessage chat

--    sendMessage
--      :: RemoteMessage
--      -> HalogenM AppState AppAction (ChildComponents t1 r ) output m Unit
    sendMessage message' = do
      let message = view Message._toWire message'
      log "Main: sending message"
      { maybeBroadcaster, id } <- H.get
      let wireEnvelope = Tuple id message
      case maybeBroadcaster of
        Nothing ->
          error "no broadcaster to send message"
        Just broadcaster -> do
          eString <- writeWire wireEnvelope
          case eString of
            Left e -> error e
            Right string ->
              broadcast broadcaster string

