module Test.Room (room_tests) where

import Prelude

import Data.Array (all, notElem, (..))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.ArrayBuffer.Class (decodeArrayBuffer, encodeArrayBuffer)
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Common (toLower)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Domination.Data.Cards as Cards
import Domination.Data.Game as Game
import Domination.Data.Room (Admission(..), Header(..), HeaderError(..), Room(..), RoomCode, admission, channelName, generatedRoomCodeLength, maxRoomCodeLength, parseHeader, parseRoomHash, printHeader, roomCode, roomCodeAlphabet, roomCodeFromIndices, roomFromHash, roomHash, scopedKey, unRoomCode)
import Data.Lens.Getter (view)
import Effect (Effect)
import Message (RemoteMessage(..), WireMessage)
import Message as Message
import Test.QuickCheck (class Testable, Result(..), assertEquals, checkResults, quickCheckPure', randomSeed)
import Test.QuickCheck.Gen (Gen, chooseInt, elements, oneOf, suchThat, vectorOf)

type EffectTest = { name :: String, test :: Effect Result }

room_tests :: Array EffectTest
room_tests =
  header_tests
    <> room_code_tests
    <> room_hash_tests
    <> storage_tests
    <> wire_compatibility_tests

-- ── Generators ──

anyCharGen :: Gen Char
anyCharGen = elements $ NEA.cons' 'a'
  $ toCharArray "bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/*#=!-_. é"

validRoomCodeCharGen :: Gen Char
validRoomCodeCharGen = elements $ NEA.cons' 'a' $ toCharArray "bcdefghijklmnopqrstuvwxyz0123456789"

stringOf :: Int -> Int -> Gen Char -> Gen String
stringOf minLength maxLength charGen = do
  n <- chooseInt minLength maxLength
  fromCharArray <$> vectorOf n charGen

roomCodeGen :: Gen RoomCode
roomCodeGen = do
  raw <- stringOf 1 maxRoomCodeLength validRoomCodeCharGen
  maybe roomCodeGen pure $ roomCode raw

roomGen :: Gen Room
roomGen = oneOf $ NEA.cons' (pure GlobalRoom) [ GameRoom <$> roomCodeGen ]

uuidGen :: Gen String
uuidGen = String.joinWith "-" <$> traverse hexString [ 8, 4, 4, 4, 12 ]
  where
    hex = elements $ NEA.cons' '0' $ toCharArray "123456789abcdef"
    hexString n = fromCharArray <$> vectorOf n hex

senderGen :: Gen String
senderGen = oneOf $ NEA.cons' uuidGen [ stringOf 1 40 anyCharGen ]

legacySenderGen :: Gen String
legacySenderGen = senderGen `suchThat` (not <<< isVersioned)

isVersioned :: String -> Boolean
isVersioned = isJust <<< String.stripPrefix (Pattern "dom/")

headerGen :: Gen Header
headerGen = oneOf $ NEA.cons'
  (LegacyHeader <$> legacySenderGen)
  [ V1Header <$> roomGen <*> senderGen ]

-- ── Header ──

header_tests :: Array EffectTest
header_tests =
  [ { name: "∀ header: parseHeader (printHeader h) = Right h"
    , test: quick_check 500 do
        h <- headerGen
        pure $ assertEquals (parseHeader $ printHeader h) (Right h)
    }
  , { name: "∀ legacy sender (e.g. a uuid): parses as a legacy header in the global room"
    , test: quick_check 300 do
        sender <- legacySenderGen
        pure $ assertEquals (parseHeader sender) (Right $ LegacyHeader sender)
    }
  , { name: "∀ version ≠ 1: dom/<v>/… is an unsupported version"
    , test: quick_check 300 do
        version <- stringOf 0 4 anyCharGen `suchThat` \v -> v /= "1" && notContains "/" v
        rest <- stringOf 0 20 anyCharGen
        pure $ assertEquals
          (parseHeader $ "dom/" <> version <> "/" <> rest)
          (Left $ UnsupportedVersion version)
    }
  , { name: "∀ s: a string starting with dom/ never parses as a legacy header"
    , test: quick_check 500 do
        rest <- stringOf 0 40 anyCharGen
        pure $ case parseHeader ("dom/" <> rest) of
          Right (LegacyHeader _) -> Failed $ "parsed as legacy: dom/" <> rest
          _ -> Success
    }
  , { name: "∀ rooms r r': admission r (printHeader (V1Header r' s)) = Admit ⇔ r = r'"
    , test: quick_check 500 do
        mine <- roomGen
        theirs <- oneOf $ NEA.cons' (pure mine) [ roomGen ]
        sender <- senderGen
        let decision = admission mine $ printHeader $ V1Header theirs sender
        pure $ assertEquals (decision == Admit) (mine == theirs)
    }
  , { name: "∀ legacy sender: admitted only in the global room"
    , test: quick_check 300 do
        mine <- roomGen
        sender <- legacySenderGen
        pure $ assertEquals (admission mine sender == Admit) (mine == GlobalRoom)
    }
  , { name: "examples from the header grammar"
    , test: pure $ all_success
        [ assertEquals (parseHeader "3b2f-uuid") (Right $ LegacyHeader "3b2f-uuid")
        , assertEquals (parseHeader "dom/1/*/3b2f") (Right $ V1Header GlobalRoom "3b2f")
        , assertEquals (parseHeader "dom/1/k7m2x9qp/3b2f") (Right $ V1Header (gameRoom "k7m2x9qp") "3b2f")
        , assertEquals (parseHeader "dom/2/lobby/3b2f") (Left $ UnsupportedVersion "2")
        , assertEquals (parseHeader "dom/x/k7m2x9qp/3b2f") (Left $ UnsupportedVersion "x")
        , assertEquals (parseHeader "dom/1/K!/3b2f") (Left $ InvalidRoomCode "K!")
        , assertEquals (parseHeader "dom/1/*/") (Left $ MalformedHeader "dom/1/*/")
        , assertEquals (parseHeader "dom/1") (Left $ MalformedHeader "dom/1")
        , assertEquals (printHeader $ V1Header (gameRoom "k7m2x9qp") "3b2f") "dom/1/k7m2x9qp/3b2f"
        , assertEquals (admission (gameRoom "abc") "dom/1/xyz/3b2f") (ForeignRoom $ gameRoom "xyz")
        , assertEquals (admission GlobalRoom "dom/9/*/3b2f") (Unsupported $ UnsupportedVersion "9")
        ]
    }
  ]

-- ── Room codes ──

room_code_tests :: Array EffectTest
room_code_tests =
  [ { name: "∀ s: roomCode accepts exactly [a-z0-9]{1,32} after lower-casing"
    , test: quick_check 1000 do
        raw <- oneOf $ NEA.cons'
          (stringOf 0 40 anyCharGen)
          [ stringOf 0 (maxRoomCodeLength + 2) validRoomCodeCharGen ]
        let
          lowered = toLower raw
          expected = between 1 maxRoomCodeLength (String.length lowered)
            && all isLowerAlphaNum (toCharArray lowered)
        pure $ assertEquals (unRoomCode <$> roomCode raw)
          (if expected then Just lowered else Nothing)
    }
  , { name: "∀ alphabet indices: generated codes are valid and 8 characters long"
    , test: quick_check 500 do
        indices <- vectorOf generatedRoomCodeLength
          $ chooseInt 0 (Array.length roomCodeAlphabet - 1)
        pure $ assertEquals
          (String.length <<< unRoomCode <$> roomCodeFromIndices indices)
          (Just generatedRoomCodeLength)
    }
  , { name: "the generation alphabet has 32 valid, distinct characters"
    , test: pure $ all_success
        [ assertEquals (Array.length roomCodeAlphabet) 32
        , assertEquals (Array.length $ Array.nub roomCodeAlphabet) 32
        , assertEquals (all isLowerAlphaNum roomCodeAlphabet) true
        ]
    }
  , { name: "an out-of-range index yields no code"
    , test: pure $ assertEquals (roomCodeFromIndices [ 0, 32 ]) Nothing
    }
  ]

isLowerAlphaNum :: Char -> Boolean
isLowerAlphaNum c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

-- ── URL fragment ──

room_hash_tests :: Array EffectTest
room_hash_tests =
  [ { name: "∀ room: roomFromHash (roomHash r) = r"
    , test: quick_check 500 do
        room <- roomGen
        pure $ assertEquals (roomFromHash $ roomHash room) room
    }
  , { name: "∀ fragment: anything that is not a valid room link is the global room"
    , test: quick_check 500 do
        fragment <- stringOf 0 40 anyCharGen
        pure $ case parseRoomHash ("#" <> fragment) of
          Right (GameRoom code) -> assertEquals fragment ("room=" <> unRoomCode code)
          Right GlobalRoom -> assertEquals fragment ""
          Left _ -> assertEquals (roomFromHash $ "#" <> fragment) GlobalRoom
    }
  , { name: "examples of room links"
    , test: pure $ all_success
        [ assertEquals (parseRoomHash "") (Right GlobalRoom)
        , assertEquals (parseRoomHash "#") (Right GlobalRoom)
        , assertEquals (parseRoomHash "#room=k7m2x9qp") (Right $ gameRoom "k7m2x9qp")
        , assertEquals (parseRoomHash "room=K7M2X9QP") (Right $ gameRoom "k7m2x9qp")
        , assertEquals (parseRoomHash "#room=<script>") (Left "room=<script>")
        , assertEquals (parseRoomHash "#other") (Left "other")
        , assertEquals (roomHash $ gameRoom "k7m2x9qp") "#room=k7m2x9qp"
        , assertEquals (roomHash GlobalRoom) ""
        , assertEquals (channelName GlobalRoom) "global-dev"
        ]
    }
  ]

-- ── Storage keys ──

globalKeys :: Array String
globalKeys =
  [ "game_state", "chat", "player-id", "username", "kingdom", "player_index"
  , "player_count", "bot_strategies", "bot_delay", "server-url"
  ] <> map (\n -> "game_state_" <> show n) (0 .. 9)

storage_tests :: Array EffectTest
storage_tests =
  [ { name: "∀ key: scopedKey GlobalRoom = identity"
    , test: quick_check 300 do
        key <- stringOf 0 20 anyCharGen
        pure $ assertEquals (scopedKey GlobalRoom key) key
    }
  , { name: "∀ (code, key) pairs: scopedKey is injective"
    , test: quick_check 500 do
        c1 <- roomCodeGen
        c2 <- oneOf $ NEA.cons' (pure c1) [ roomCodeGen ]
        k1 <- elements $ NEA.cons' "game_state" globalKeys
        k2 <- oneOf $ NEA.cons' (pure k1) [ elements $ NEA.cons' "chat" globalKeys ]
        let
          sameInput = c1 == c2 && k1 == k2
          sameOutput = scopedKey (GameRoom c1) k1 == scopedKey (GameRoom c2) k2
        pure $ assertEquals sameOutput sameInput
    }
  , { name: "∀ room code: room keys never collide with a global key"
    , test: quick_check 300 do
        code <- roomCodeGen
        pure $ assertEquals
          (all (\key -> scopedKey (GameRoom code) key `notElem` globalKeys) globalKeys)
          true
    }
  , { name: "room keys live under room/<code>/"
    , test: pure $ assertEquals (scopedKey (gameRoom "abc") "game_state") "room/abc/game_state"
    }
  ]

-- ── Wire compatibility ──

wire_compatibility_tests :: Array EffectTest
wire_compatibility_tests = do
  Tuple label message <-
    [ Tuple "chat" $ ChatMessage { username: "id", message: "hello", chatNumber: 3 }
    , Tuple "join" $ JoinMessage { clientId: "id", timestamp: 12345 }
    , Tuple "game" $ GameMessage { i: 7, state: Game.new 2 Cards.cardMap true, playMade: Nothing }
    ]
  header <-
    [ printHeader $ V1Header GlobalRoom "3b2f-uuid"
    , printHeader $ V1Header (gameRoom "k7m2x9qp") "3b2f-uuid"
    ]
  let wire = view Message._toWire message
  [ { name: "today's envelope decoder reads a " <> label <> " message sent with " <> header
    , test: do
        buffer <- encodeArrayBuffer (Tuple header wire)
        decoded :: Maybe (Tuple String WireMessage) <- decodeArrayBuffer buffer
        pure $ assertEquals decoded (Just $ Tuple header wire)
    }
  , { name: "the header of a " <> label <> " message can be read before its body (" <> header <> ")"
    , test: do
        buffer <- encodeArrayBuffer (Tuple header wire)
        decoded :: Maybe String <- decodeArrayBuffer buffer
        pure $ assertEquals decoded (Just header)
    }
  ]

-- ── Helpers ──

gameRoom :: String -> Room
gameRoom = maybe GlobalRoom GameRoom <<< roomCode

notContains :: String -> String -> Boolean
notContains needle = not <<< String.contains (Pattern needle)

all_success :: Array Result -> Result
all_success results = maybe Success identity $ Array.find isFailure results
  where
    isFailure = case _ of
      Failed _ -> true
      Success -> false

quick_check :: forall prop. Testable prop => Int -> prop -> Effect Result
quick_check trials prop = do
  seed <- randomSeed
  pure $ summarize $ checkResults $ quickCheckPure' seed trials prop
  where
    summarize { failures } = maybe Success
      (\{ message, seed } -> Failed $ message <> " (seed " <> show seed <> ")")
      (List.head failures)
