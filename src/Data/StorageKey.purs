-- | Every localStorage key Domination writes, as one closed type. Saving,
-- | loading and the Storage panel share these names, so a new key gets its
-- | description where it is added.
module Domination.Data.StorageKey
  ( DeviceKey(..)
  , KeyDescription
  , RoomKey(..)
  , StorageKey(..)
  , allDeviceKeys
  , describe
  , deviceKeyName
  , isHeldInMemory
  , parseKey
  , printKey
  , roomKeyName
  , undoStep
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array (find)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), stripPrefix)
import Data.String as String
import DebugLog (consoleLogKey, syncJournalKey)
import Domination.Capability.ClearGeneration (generationKey)
import Domination.Data.Room (Room(..), roomCode, scopedKey, unRoomCode)
import Domination.Data.Undo (historySlot, historySlots)

-- | State saved separately for each room.
data RoomKey = CurrentGame | UndoStep Int | Chat | MailboxSync | GameVersion

-- | State saved once per browser, whatever the room.
data DeviceKey
  = PlayerId
  | Username
  | Kingdom
  | PlayerIndex
  | PlayerCount
  | BotStrategies
  | BotDelay
  | ServerUrl
  | MailboxUrl
  | RenameProposals
  | ConsoleLog
  | SyncJournal
  | LegacyRelaySeed
  | ClearGeneration

data StorageKey = InRoom Room RoomKey | OnDevice DeviceKey

derive instance eqRoomKey :: Eq RoomKey
derive instance genericRoomKey :: Generic RoomKey _
instance showRoomKey :: Show RoomKey where
  show = genericShow

derive instance eqDeviceKey :: Eq DeviceKey
derive instance genericDeviceKey :: Generic DeviceKey _
instance showDeviceKey :: Show DeviceKey where
  show = genericShow

derive instance eqStorageKey :: Eq StorageKey
derive instance genericStorageKey :: Generic StorageKey _
instance showStorageKey :: Show StorageKey where
  show = genericShow

type KeyDescription = { label :: String, consequence :: String }

-- | The undo history slot that keeps `step`.
undoStep :: Int -> RoomKey
undoStep = UndoStep <<< historySlot

allDeviceKeys :: Array DeviceKey
allDeviceKeys =
  [ PlayerId, Username, Kingdom, PlayerIndex, PlayerCount, BotStrategies, BotDelay
  , ServerUrl, MailboxUrl, RenameProposals, ConsoleLog, SyncJournal, LegacyRelaySeed
  , ClearGeneration
  ]

undoPrefix :: String
undoPrefix = "game_state_"

roomPrefix :: String
roomPrefix = "room/"

roomKeyName :: RoomKey -> String
roomKeyName = case _ of
  CurrentGame -> "game_state"
  UndoStep slot -> undoPrefix <> show slot
  Chat -> "chat"
  MailboxSync -> "mailbox_sync"
  GameVersion -> "game_version"

deviceKeyName :: DeviceKey -> String
deviceKeyName = case _ of
  PlayerId -> "player-id"
  Username -> "username"
  Kingdom -> "kingdom"
  PlayerIndex -> "player_index"
  PlayerCount -> "player_count"
  BotStrategies -> "bot_strategies"
  BotDelay -> "bot_delay"
  ServerUrl -> "server-url"
  MailboxUrl -> "mailbox-url"
  RenameProposals -> "rename_proposals"
  ConsoleLog -> consoleLogKey
  SyncJournal -> syncJournalKey
  LegacyRelaySeed -> "seed"
  ClearGeneration -> generationKey

printKey :: StorageKey -> String
printKey = case _ of
  InRoom room key -> scopedKey room $ roomKeyName key
  OnDevice key -> deviceKeyName key

-- | The inverse of `printKey`: only the exact strings it produces parse.
parseKey :: String -> Maybe StorageKey
parseKey name = case stripPrefix (Pattern roomPrefix) name of
  Just rest -> parseGameRoomKey rest
  Nothing -> (InRoom GlobalRoom <$> parseRoomKey name) <|> (OnDevice <$> parseDeviceKey name)

parseGameRoomKey :: String -> Maybe StorageKey
parseGameRoomKey rest = do
  { before, after } <- splitAtSlash rest
  code <- roomCode before
  guard $ unRoomCode code == before
  InRoom (GameRoom code) <$> parseRoomKey after

splitAtSlash :: String -> Maybe { before :: String, after :: String }
splitAtSlash text = do
  at <- String.indexOf (Pattern "/") text
  pure { before: String.take at text, after: String.drop (at + 1) text }

parseRoomKey :: String -> Maybe RoomKey
parseRoomKey name = find ((_ == name) <<< roomKeyName) [ CurrentGame, Chat, MailboxSync, GameVersion ]
  <|> (stripPrefix (Pattern undoPrefix) name >>= parseUndoSlot)

parseUndoSlot :: String -> Maybe RoomKey
parseUndoSlot digits = do
  slot <- fromString digits
  guard $ slot >= 0 && slot < historySlots && show slot == digits
  pure $ UndoStep slot

parseDeviceKey :: String -> Maybe DeviceKey
parseDeviceKey name = find ((_ == name) <<< deviceKeyName) allDeviceKeys

-- | Whether a running page keeps this key's value in memory and writes it
-- | back on its next change, so deleting it must reload the page.
isHeldInMemory :: StorageKey -> Boolean
isHeldInMemory = case _ of
  InRoom _ (UndoStep _) -> false
  InRoom _ _ -> true
  OnDevice key -> isPreference key

isPreference :: DeviceKey -> Boolean
isPreference = case _ of
  ConsoleLog -> false
  SyncJournal -> false
  LegacyRelaySeed -> false
  ClearGeneration -> false
  _ -> true

describe :: StorageKey -> KeyDescription
describe = case _ of
  InRoom _ key -> describeRoomKey key
  OnDevice key -> describeDeviceKey key

describeRoomKey :: RoomKey -> KeyDescription
describeRoomKey = case _ of
  CurrentGame ->
    { label: "Current game"
    , consequence: "The room opens without a saved game; in a game room, its mailbox or other players can send it back."
    }
  UndoStep slot ->
    { label: "Undo step " <> show slot
    , consequence: "Undo can't go back to this step."
    }
  Chat ->
    { label: "Chat"
    , consequence: "The room's chat history is gone."
    }
  MailboxSync ->
    { label: "Mailbox sync"
    , consequence: "Sync bookkeeping is rebuilt; a move the mailbox hasn't confirmed yet is forgotten."
    }
  GameVersion ->
    { label: "Game version"
    , consequence: "Sync bookkeeping is rebuilt from the next game received."
    }

describeDeviceKey :: DeviceKey -> KeyDescription
describeDeviceKey = case _ of
  PlayerId ->
    { label: "Player ID"
    , consequence: "A new identity is created; other players see a new name entry."
    }
  Username ->
    { label: "Name"
    , consequence: "A random name is chosen."
    }
  Kingdom ->
    { label: "Kingdom"
    , consequence: "The kingdom returns to its default cards."
    }
  PlayerIndex ->
    { label: "Your seat"
    , consequence: "Game setup returns to seat 1."
    }
  PlayerCount ->
    { label: "Player count"
    , consequence: "Game setup returns to its default player count."
    }
  BotStrategies ->
    { label: "Bots"
    , consequence: "Game setup returns to no bots."
    }
  BotDelay ->
    { label: "Bot delay"
    , consequence: "Bots return to their default speed."
    }
  ServerUrl ->
    { label: "Relay server"
    , consequence: "The default relay server is used."
    }
  MailboxUrl ->
    { label: "Mailbox"
    , consequence: "The default mailbox is used."
    }
  RenameProposals ->
    { label: "Rename proposals"
    , consequence: "Your card rename proposals are gone."
    }
  ConsoleLog ->
    { label: "Console log"
    , consequence: "Older log lines are gone from bug reports; new lines are saved right away."
    }
  SyncJournal ->
    { label: "Sync journal"
    , consequence: "Older sync history is gone from bug reports; new lines are saved right away."
    }
  LegacyRelaySeed ->
    { label: "Old relay seed"
    , consequence: "Nothing: only older versions of Domination used it."
    }
  ClearGeneration ->
    { label: "Clear counter"
    , consequence: "Kept, so deleted data stays deleted in every open tab."
    }
