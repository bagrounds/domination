module AppState where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Lens.Lens (Lens', Lens)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Domination.Capability.Broadcast (Broadcaster)
import Domination.Data.Card (Card)
import Domination.Data.Cards as Cards
import Domination.Data.Stack (_card)
import Message (RemoteMessage)

type AppState =
  { connectionCount :: Int
  , id :: String
  , username :: String
  , usernames :: HashMap String String
  , message :: String
  , messages :: Array RemoteMessage
  , gameOn :: Boolean
  , maybeBroadcaster :: Maybe Broadcaster
  , roomCode :: String
  , dominationConfig :: Config
  , showMenu :: Boolean
  }

_id :: Lens' AppState String
_id = prop (SProxy :: SProxy "id")
_messages :: Lens' AppState (Array RemoteMessage)
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
_dominationConfig :: Lens' AppState Config
_dominationConfig = prop (SProxy :: SProxy "dominationConfig")
_showMenu
  :: forall a b r
  . Lens { showMenu :: a | r } { showMenu :: b | r } a b
_showMenu = prop (SProxy :: SProxy "showMenu")

globalRoomCode :: String
globalRoomCode = "global-dev"

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
  , dominationConfig: newConfig
  , showMenu: false
  }

newConfig :: Config
newConfig =
  { nextPlayerCount: one
  , nextPlayerIndex: zero
  , kingdom: defaultKingdom
  }

defaultKingdom :: Array { card :: Card, selected :: Boolean }
defaultKingdom = ({ card: _, selected: true }) <$> Cards.cardMap

type Selection = { card :: Card, selected :: Boolean }

type Config =
  { nextPlayerIndex :: Int
  , nextPlayerCount :: Int
  , kingdom :: Array Selection
  }

upgradeSelection :: Selection -> Selection
upgradeSelection = _card %~ Cards.upgrade

upgradeConfig :: Config -> Config
upgradeConfig = _kingdom %~ map upgradeSelection

_nextPlayerIndex :: Lens' Config Int
_nextPlayerIndex = prop (SProxy :: SProxy "nextPlayerIndex")
_nextPlayerCount :: Lens' Config Int
_nextPlayerCount = prop (SProxy :: SProxy "nextPlayerCount")
_kingdom
  :: forall a b r
  . Lens { kingdom :: a | r } { kingdom :: b | r } a b
_kingdom = prop (SProxy :: SProxy "kingdom")

