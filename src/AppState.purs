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
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Domination.Capability.Broadcast.Bugout (BugoutBroadcaster)
import Domination.Data.Card (Card, CardSpec(..))
import Domination.Data.Cards as Cards
import Message (RemoteMessage)

type AppState =
  { connectionCount :: Int
  , id :: String
  , announce :: String
  , username :: String
  , usernames :: HashMap String String
  , message :: String
  , messages :: Array RemoteMessage
  , chatNumber :: Int
  , gameOn :: Boolean
  , maybeBroadcaster :: Maybe BugoutBroadcaster
  , roomCode :: String
  , dominationConfig :: Config
  , showMenu :: Boolean
  , longGame :: Boolean
  , maybeAudioContext :: Maybe AudioContext
  }

_card :: Lens' CardSpec Card
_card = lens' f
  where
    f :: CardSpec -> Tuple Card (Card -> CardSpec)
    f (IndependentCard c) = Tuple c IndependentCard
    f (CardWithRequirements { card: c, requirements }) = Tuple c (\c1 -> CardWithRequirements { card: c1, requirements })


_cardSpec :: Lens' CardSpecSelection CardSpec
_cardSpec = (prop (SProxy :: SProxy "cardSpec"))

_cardSpecCard :: Lens' CardSpecSelection Card
_cardSpecCard = _cardSpec <<< _card

_id :: Lens' AppState String
_id = prop (SProxy :: SProxy "id")
_messages :: Lens' AppState (Array RemoteMessage)
_messages = prop (SProxy :: SProxy "messages")
_message :: Lens' AppState String
_message = prop (SProxy :: SProxy "message")
_chatNumber :: Lens' AppState Int
_chatNumber = prop (SProxy :: SProxy "chatNumber")
_connectionCount :: Lens' AppState Int
_connectionCount = prop (SProxy :: SProxy "connectionCount")
_usernames :: Lens' AppState (HashMap String String)
_usernames = prop (SProxy :: SProxy "usernames")
_username :: Lens' AppState String
_username = prop (SProxy :: SProxy "username")
_announce :: Lens' AppState String
_announce = prop (SProxy :: SProxy "announce")
_maybeBroadcaster :: Lens' AppState (Maybe BugoutBroadcaster)
_maybeBroadcaster = prop (SProxy :: SProxy "maybeBroadcaster")
_dominationConfig :: Lens' AppState Config
_dominationConfig = prop (SProxy :: SProxy "dominationConfig")
_showMenu
  :: forall a b r
  . Lens { showMenu :: a | r } { showMenu :: b | r } a b
_showMenu = prop (SProxy :: SProxy "showMenu")
_maybeAudioContext
  :: forall a b r
  . Lens
    { maybeAudioContext :: a | r }
    { maybeAudioContext :: b | r }
    a b
_maybeAudioContext = prop (SProxy :: SProxy "maybeAudioContext")

globalRoomCode :: String
globalRoomCode = "global-dev"

defaultAnnounce :: String
defaultAnnounce = "wss://p2p-tracker-24is.onrender.com"

newApp :: AppState
newApp =
  { connectionCount: 0
  , id: ""
  , username: ""
  , announce: defaultAnnounce
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
  }

newConfig :: Config
newConfig =
  { nextPlayerCount: one
  , nextPlayerIndex: zero
  , kingdom: defaultKingdom
  , longGame: false
  }

type CardSpecSelection = { cardSpec :: CardSpec, selected :: Boolean }

defaultKingdom :: Array CardSpecSelection
defaultKingdom = ({ cardSpec: _, selected: true }) <$> Cards.cardSpecMap

type Selection = { card :: Card, selected :: Boolean }

_selected
  :: forall a b r
  . Lens { selected :: a | r } { selected :: b | r } a b
_selected = prop (SProxy :: SProxy "selected")

type Config =
  { nextPlayerIndex :: Int
  , nextPlayerCount :: Int
  , kingdom :: Array CardSpecSelection
  , longGame :: Boolean
  }

upgradeSelection :: CardSpecSelection -> CardSpecSelection
upgradeSelection = _cardSpecCard %~ Cards.upgrade

upgradeConfig :: Config -> Config
upgradeConfig = _kingdom %~ map upgradeSelection

_nextPlayerIndex :: Lens' Config Int
_nextPlayerIndex = prop (SProxy :: SProxy "nextPlayerIndex")
_nextPlayerCount :: Lens' Config Int
_nextPlayerCount = prop (SProxy :: SProxy "nextPlayerCount")
_longGame
  :: forall a b r
  . Lens { longGame :: a | r } { longGame :: b | r } a b
_longGame = prop (SProxy :: SProxy "longGame")
_kingdom
  :: forall a b r
  . Lens { kingdom :: a | r } { kingdom :: b | r } a b
_kingdom = prop (SProxy :: SProxy "kingdom")
