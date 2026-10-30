--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a data type for the application state of an online game or chat platform.
--|
--| ### Key Concepts
--| * **State Management**: The concept of managing state in the application.
--| * **Data Lensing**: The use of data lenses to abstractly access and manipulate values within complex data structures.
--| * **Configurable Game State**: The design of a game with configurable state (e.g., game mode, rules) using enums and functions.
module AppState where

import Prelude

import Audio.WebAudio.Types (AudioContext)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Lens (lens')
import Data.Lens.Lens (Lens', Lens)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Array (deleteAt, length, snoc)
import Data.Maybe (Maybe(..), maybe)
import Type.Proxy (Proxy(..))
import Data.Tuple (Tuple(..))
import Domination.Capability.Broadcast.WebSocket (WebSocketBroadcaster)
import Domination.Data.AI.Strategy (Strategy)
import Domination.Data.Card (Card, CardSpec(..))
import Domination.Data.Cards as Cards
import Message (RemoteMessage)

type ClientInfo =
  { timestamp :: Int
  , clientId :: String
  }

data SettingsTab
  = GameSetupTab
  | BotsTab
  | KingdomTab
  | ConnectionTab
  | DebugTab

derive instance eqSettingsTab :: Eq SettingsTab

settingsTabs :: Array SettingsTab
settingsTabs = [ GameSetupTab, BotsTab, KingdomTab, ConnectionTab, DebugTab ]

settingsTabLabel :: SettingsTab -> String
settingsTabLabel = case _ of
  GameSetupTab -> "Game"
  BotsTab -> "Bots"
  KingdomTab -> "Kingdom"
  ConnectionTab -> "Connection"
  DebugTab -> "Debug"

type AppState =
  { connectionCount :: Int
  , id :: String
  , serverUrl :: String
  , username :: String
  , usernames :: HashMap String String
  , message :: String
  , messages :: Array RemoteMessage
  , chatNumber :: Int
  , gameOn :: Boolean
  , maybeBroadcaster :: Maybe WebSocketBroadcaster
  , roomCode :: String
  , dominationConfig :: Config
  , showMenu :: Boolean
  , longGame :: Boolean
  , maybeAudioContext :: Maybe AudioContext
  , connectedClients :: HashMap String ClientInfo
  , heartbeatInterval :: Int
  , heartbeatTimeout :: Int
  , settingsTab :: SettingsTab
  , debugReport :: String
  }

_card :: Lens' CardSpec Card
_card = lens' f
  where
    f :: CardSpec -> Tuple Card (Card -> CardSpec)
    f (IndependentCard c) = Tuple c IndependentCard
    f (CardWithRequirements { card: c, requirements }) = Tuple c (\c1 -> CardWithRequirements { card: c1, requirements })


_cardSpec :: Lens' CardSpecSelection CardSpec
_cardSpec = (prop (Proxy :: Proxy "cardSpec"))

_cardSpecCard :: Lens' CardSpecSelection Card
_cardSpecCard = _cardSpec <<< _card

_id :: Lens' AppState String
_id = prop (Proxy :: Proxy "id")
_messages :: Lens' AppState (Array RemoteMessage)
_messages = prop (Proxy :: Proxy "messages")
_message :: Lens' AppState String
_message = prop (Proxy :: Proxy "message")
_chatNumber :: Lens' AppState Int
_chatNumber = prop (Proxy :: Proxy "chatNumber")
_connectionCount :: Lens' AppState Int
_connectionCount = prop (Proxy :: Proxy "connectionCount")
_usernames :: Lens' AppState (HashMap String String)
_usernames = prop (Proxy :: Proxy "usernames")
_username :: Lens' AppState String
_username = prop (Proxy :: Proxy "username")
_serverUrl :: Lens' AppState String
_serverUrl = prop (Proxy :: Proxy "serverUrl")
_maybeBroadcaster :: Lens' AppState (Maybe WebSocketBroadcaster)
_maybeBroadcaster = prop (Proxy :: Proxy "maybeBroadcaster")
_dominationConfig :: Lens' AppState Config
_dominationConfig = prop (Proxy :: Proxy "dominationConfig")
_showMenu
  :: forall a b r
  . Lens { showMenu :: a | r } { showMenu :: b | r } a b
_showMenu = prop (Proxy :: Proxy "showMenu")
_maybeAudioContext
  :: forall a b r
  . Lens
    { maybeAudioContext :: a | r }
    { maybeAudioContext :: b | r }
    a b
_maybeAudioContext = prop (Proxy :: Proxy "maybeAudioContext")

_connectedClients :: Lens' AppState (HashMap String ClientInfo)
_connectedClients = prop (Proxy :: Proxy "connectedClients")

_heartbeatInterval :: Lens' AppState Int
_heartbeatInterval = prop (Proxy :: Proxy "heartbeatInterval")

_heartbeatTimeout :: Lens' AppState Int
_heartbeatTimeout = prop (Proxy :: Proxy "heartbeatTimeout")

_settingsTab :: Lens' AppState SettingsTab
_settingsTab = prop (Proxy :: Proxy "settingsTab")

_debugReport :: Lens' AppState String
_debugReport = prop (Proxy :: Proxy "debugReport")

globalRoomCode :: String
globalRoomCode = "global-dev"

defaultServerUrl :: String
defaultServerUrl = "wss://purescript-wip.onrender.com"

defaultHeartbeatInterval :: Int
defaultHeartbeatInterval = 5000

defaultHeartbeatTimeout :: Int
defaultHeartbeatTimeout = 15000

newApp :: AppState
newApp =
  { connectionCount: 0
  , id: ""
  , username: ""
  , serverUrl: defaultServerUrl
  , usernames: HashMap.empty
  , message: ""
  , messages: []
  , chatNumber: 0
  , gameOn: false
  , maybeBroadcaster: Nothing
  , roomCode: globalRoomCode
  , dominationConfig: newConfig
  , showMenu: false
  , longGame: false
  , maybeAudioContext: Nothing
  , connectedClients: HashMap.empty
  , heartbeatInterval: defaultHeartbeatInterval
  , heartbeatTimeout: defaultHeartbeatTimeout
  , settingsTab: GameSetupTab
  , debugReport: ""
  }

newConfig :: Config
newConfig =
  { nextPlayerCount: one
  , nextPlayerIndex: zero
  , kingdom: defaultKingdom
  , longGame: false
  , botStrategies: []
  , botDelay: 2000
  }

type CardSpecSelection = { cardSpec :: CardSpec, selected :: Boolean }

defaultKingdom :: Array CardSpecSelection
defaultKingdom = ({ cardSpec: _, selected: true }) <$> Cards.cardSpecMap

type Selection = { card :: Card, selected :: Boolean }

_selected
  :: forall a b r
  . Lens { selected :: a | r } { selected :: b | r } a b
_selected = prop (Proxy :: Proxy "selected")

type Config =
  { nextPlayerIndex :: Int
  , nextPlayerCount :: Int
  , kingdom :: Array CardSpecSelection
  , longGame :: Boolean
  , botStrategies :: Array Strategy
  , botDelay :: Int
  }

maxBotDelay :: Int
maxBotDelay = 5000

clampBotDelay :: Int -> Int
clampBotDelay = clamp zero maxBotDelay

type Roster r =
  { nextPlayerIndex :: Int
  , nextPlayerCount :: Int
  , botStrategies :: Array Strategy
  | r
  }

-- | Every roster keeps a seat for the local human and one for each bot,
-- | with the human seated inside the table.
normalizeRoster :: forall r. Roster r -> Roster r
normalizeRoster roster = roster
  { nextPlayerCount = playerCount
  , nextPlayerIndex = clamp zero (playerCount - one) roster.nextPlayerIndex
  }
  where
    playerCount = max (length roster.botStrategies + one) roster.nextPlayerCount

addBot :: forall r. Strategy -> Roster r -> Roster r
addBot strategy roster = normalizeRoster roster
  { botStrategies = snoc roster.botStrategies strategy
  , nextPlayerCount = roster.nextPlayerCount + one
  }

removeBot :: forall r. Int -> Roster r -> Roster r
removeBot botIndex roster = maybe roster
  ( \botStrategies -> normalizeRoster roster
    { botStrategies = botStrategies
    , nextPlayerCount = roster.nextPlayerCount - one
    }
  )
  (deleteAt botIndex roster.botStrategies)

setPlayerCount :: forall r. Int -> Roster r -> Roster r
setPlayerCount playerCount roster =
  normalizeRoster roster { nextPlayerCount = playerCount }

setPlayerIndex :: forall r. Int -> Roster r -> Roster r
setPlayerIndex playerIndex roster = normalizeRoster roster
  { nextPlayerIndex = max zero playerIndex
  , nextPlayerCount = max (playerIndex + one) roster.nextPlayerCount
  }

upgradeSelection :: CardSpecSelection -> CardSpecSelection
upgradeSelection = _cardSpecCard %~ Cards.upgrade

upgradeConfig :: Config -> Config
upgradeConfig = _kingdom %~ map upgradeSelection

_nextPlayerIndex :: Lens' Config Int
_nextPlayerIndex = prop (Proxy :: Proxy "nextPlayerIndex")
_nextPlayerCount :: Lens' Config Int
_nextPlayerCount = prop (Proxy :: Proxy "nextPlayerCount")
_longGame
  :: forall a b r
  . Lens { longGame :: a | r } { longGame :: b | r } a b
_longGame = prop (Proxy :: Proxy "longGame")
_kingdom
  :: forall a b r
  . Lens { kingdom :: a | r } { kingdom :: b | r } a b
_kingdom = prop (Proxy :: Proxy "kingdom")

_botStrategies
  :: forall a b r
  . Lens { botStrategies :: a | r } { botStrategies :: b | r } a b
_botStrategies = prop (Proxy :: Proxy "botStrategies")

_botDelay
  :: forall a b r
  . Lens { botDelay :: a | r } { botDelay :: b | r } a b
_botDelay = prop (Proxy :: Proxy "botDelay")
