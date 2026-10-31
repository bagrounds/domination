--| ## Rooms
--|
--| A room is a group of clients that share one game, chat and presence count.
--| The relay forwards every message to every client, so rooms are enforced by
--| the receiver: each envelope carries a versioned header in its first field,
--| and clients drop messages addressed to another room.
--|
--| See docs/2026-10-31-matchmaking-design.md for the design.
module Domination.Data.Room
  ( RoomCode
  , roomCode
  , unRoomCode
  , roomCodeAlphabet
  , generatedRoomCodeLength
  , maxRoomCodeLength
  , roomCodeFromIndices
  , generateRoomCode
  , Room(..)
  , isGameRoom
  , Header(..)
  , headerRoom
  , headerSender
  , HeaderError(..)
  , Admission(..)
  , printHeader
  , parseHeader
  , admission
  , scopedKey
  , parseRoomHash
  , roomFromHash
  , roomHash
  , channelName
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array (all, index, replicate)
import Data.Array as Array
import Data.Either (Either(..), either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), stripPrefix)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.CodeUnits as CodeUnits
import Data.String.Common (toLower)
import Data.Traversable (traverse)
import Domination.Capability.Random (class Random, randomIntBetween)

newtype RoomCode = RoomCode String

derive instance eqRoomCode :: Eq RoomCode
derive instance ordRoomCode :: Ord RoomCode

instance showRoomCode :: Show RoomCode where
  show (RoomCode code) = "(RoomCode " <> show code <> ")"

unRoomCode :: RoomCode -> String
unRoomCode (RoomCode code) = code

maxRoomCodeLength :: Int
maxRoomCodeLength = 32

generatedRoomCodeLength :: Int
generatedRoomCodeLength = 8

-- | Crockford-style lower-case alphabet: no i, l, o or u, so codes survive
-- | being read aloud or retyped. It is a strict subset of the characters
-- | `roomCode` accepts, so generation can change without breaking parsers.
roomCodeAlphabet :: Array Char
roomCodeAlphabet = toCharArray "0123456789abcdefghjkmnpqrstvwxyz"

isRoomCodeChar :: Char -> Boolean
isRoomCodeChar c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

hasRoomCodeLength :: String -> Boolean
hasRoomCodeLength code = between 1 maxRoomCodeLength $ CodeUnits.length code

-- | Accepts exactly `[a-z0-9]{1,32}` after lower-casing.
roomCode :: String -> Maybe RoomCode
roomCode raw = RoomCode code <$ guard isValid
  where
    code = toLower raw
    isValid = hasRoomCodeLength code && all isRoomCodeChar (toCharArray code)

roomCodeFromIndices :: Array Int -> Maybe RoomCode
roomCodeFromIndices = traverse (index roomCodeAlphabet) >=> fromCharArray >>> roomCode

generateRoomCode :: forall m. Random m => m (Maybe RoomCode)
generateRoomCode = roomCodeFromIndices <$> traverse (const randomAlphabetIndex)
  (replicate generatedRoomCodeLength unit)
  where
    randomAlphabetIndex = randomIntBetween 0 (Array.length roomCodeAlphabet - 1)

data Room = GlobalRoom | GameRoom RoomCode

derive instance eqRoom :: Eq Room
derive instance genericRoom :: Generic Room _
instance showRoom :: Show Room where
  show = genericShow

isGameRoom :: Room -> Boolean
isGameRoom = case _ of
  GlobalRoom -> false
  GameRoom _ -> true

-- | The first field of every wire envelope.
-- |
-- | ```text
-- | header           = versioned-header / legacy-header
-- | versioned-header = "dom/" version "/" rest
-- | v1-header        = "dom/1/" room "/" sender
-- | room             = "*" / room-code
-- | legacy-header    = sender   ; only when the string does NOT start with "dom/"
-- | ```
data Header
  = LegacyHeader String
  | V1Header Room String

derive instance eqHeader :: Eq Header
derive instance genericHeader :: Generic Header _
instance showHeader :: Show Header where
  show = genericShow

headerRoom :: Header -> Room
headerRoom = case _ of
  LegacyHeader _ -> GlobalRoom
  V1Header room _ -> room

headerSender :: Header -> String
headerSender = case _ of
  LegacyHeader sender -> sender
  V1Header _ sender -> sender

data HeaderError
  = UnsupportedVersion String
  | InvalidRoomCode String
  | MalformedHeader String

derive instance eqHeaderError :: Eq HeaderError
derive instance genericHeaderError :: Generic HeaderError _
instance showHeaderError :: Show HeaderError where
  show = genericShow

data Admission
  = Admit
  | ForeignRoom Room
  | Unsupported HeaderError

derive instance eqAdmission :: Eq Admission
derive instance genericAdmission :: Generic Admission _
instance showAdmission :: Show Admission where
  show = genericShow

versionedPrefix :: String
versionedPrefix = "dom/"

globalRoomSegment :: String
globalRoomSegment = "*"

printRoomSegment :: Room -> String
printRoomSegment = case _ of
  GlobalRoom -> globalRoomSegment
  GameRoom code -> unRoomCode code

printHeader :: Header -> String
printHeader = case _ of
  LegacyHeader sender -> sender
  V1Header room sender ->
    versionedPrefix <> "1/" <> printRoomSegment room <> "/" <> sender

-- | Parsing is ordered: a string that starts with `dom/` must be a valid
-- | versioned header, so the two forms never overlap.
parseHeader :: String -> Either HeaderError Header
parseHeader raw = maybe
  (Right $ LegacyHeader raw)
  parseVersioned
  (stripPrefix (Pattern versionedPrefix) raw)
  where
    malformed = MalformedHeader raw

    parseVersioned rest = do
      { before: version, after } <- note malformed $ splitOnce rest
      case version of
        "1" -> parseV1 after
        _ -> Left $ UnsupportedVersion version

    parseV1 rest = do
      { before: roomSegment, after: sender } <- note malformed $ splitOnce rest
      room <- parseRoomSegment roomSegment
      V1Header room <$> nonEmpty sender

    nonEmpty = case _ of
      "" -> Left malformed
      sender -> Right sender

parseRoomSegment :: String -> Either HeaderError Room
parseRoomSegment segment
  | segment == globalRoomSegment = Right GlobalRoom
  | otherwise = GameRoom <$> note (InvalidRoomCode segment) (roomCode segment)

splitOnce :: String -> Maybe { before :: String, after :: String }
splitOnce s = (\i -> { before: CodeUnits.take i s, after: CodeUnits.drop (i + 1) s })
  <$> CodeUnits.indexOf (Pattern "/") s

-- | The receiver's decision about a raw header, given the room it is in.
admission :: Room -> String -> Admission
admission myRoom = either Unsupported (headerRoom >>> admitInto myRoom) <<< parseHeader

admitInto :: Room -> Room -> Admission
admitInto myRoom senderRoom
  | myRoom == senderRoom = Admit
  | otherwise = ForeignRoom senderRoom

-- | Storage key for room state. Global keys are unchanged, so no migration is
-- | needed; room keys live under a `room/` prefix that no global key uses.
scopedKey :: Room -> String -> String
scopedKey = case _ of
  GlobalRoom -> identity
  GameRoom code -> append ("room/" <> unRoomCode code <> "/")

roomHashPrefix :: String
roomHashPrefix = "room="

-- | `location.hash` → room. `Left` carries a fragment that is not a valid room.
parseRoomHash :: String -> Either String Room
parseRoomHash hash = case withoutHash of
  "" -> Right GlobalRoom
  fragment -> note fragment
    $ GameRoom <$> (stripPrefix (Pattern roomHashPrefix) fragment >>= roomCode)
  where
    withoutHash = maybe hash identity $ stripPrefix (Pattern "#") hash

roomFromHash :: String -> Room
roomFromHash = either (const GlobalRoom) identity <<< parseRoomHash

roomHash :: Room -> String
roomHash = case _ of
  GlobalRoom -> ""
  GameRoom code -> "#" <> roomHashPrefix <> unRoomCode code

-- | Channel name handed to the transport. The WebSocket relay ignores it;
-- | a Bugout transport uses it as the swarm name.
channelName :: Room -> String
channelName = case _ of
  GlobalRoom -> "global-dev"
  GameRoom code -> "global-dev/" <> unRoomCode code
