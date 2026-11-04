--| Which copy of a game is newer, where there is no room mailbox (the global
--| room, or a room whose mailbox is turned off).
--|
--| Each copy of the game carries a `Version`, a Lamport timestamp: every
--| change a client makes (a play, an undo, a new game, a loaded game) gets a
--| time one greater than the newest version it has seen, and the id of the
--| client that made it. Versions are totally ordered by time, then origin, so
--| "ahead" is well defined and every client agrees on it. A client that comes
--| back with a game saved long ago holds an old version, so nobody adopts it,
--| and it adopts the newer game the others offer.
--|
--| The version rides in the sender segment of the envelope header, which old
--| builds read as an opaque id, so the wire layout does not change.
module Domination.Data.LogicalClock
  ( Version(..)
  , versionTime
  , tick
  , savedBeforeVersions
  , Verdict(..)
  , judge
  , printVersion
  , parseVersion
  , versionSender
  , senderVersion
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Array (all)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), indexOf, lastIndexOf, stripPrefix)
import Data.String.CodeUnits as CodeUnits

-- | `Version time origin`. The derived order compares time first.
data Version = Version Int String

derive instance eqVersion :: Eq Version
derive instance ordVersion :: Ord Version
derive instance genericVersion :: Generic Version _
instance showVersion :: Show Version where
  show = genericShow

instance encodeJsonVersion :: EncodeJson Version where
  encodeJson (Version time origin) = encodeJson { time, origin }

instance decodeJsonVersion :: DecodeJson Version where
  decodeJson json = fromRecord <$> decodeJson json
    where
      fromRecord :: { time :: Int, origin :: String } -> Version
      fromRecord { time, origin } = Version time origin

versionTime :: Version -> Int
versionTime (Version time _) = time

-- | The version of a change `origin` makes to a game at `current`.
tick :: String -> Maybe Version -> Version
tick origin current = Version (maybe 0 versionTime current + 1) origin

-- | A game saved before versions existed has only its move counter, the best
-- | estimate of its time. The empty origin lets a real change at the same
-- | time win.
savedBeforeVersions :: Int -> Version
savedBeforeVersions moves = Version moves ""

-- | What to do with a game frame from the relay.
-- | `TakeTheirs v`: it is ahead of ours; take it, and ours is now at `v`.
-- | `SendOurs`: ours is ahead; send ours so the sender catches up.
-- | `AlreadyHave`: it is the game we already have.
data Verdict = TakeTheirs Version | SendOurs | AlreadyHave

derive instance eqVerdict :: Eq Verdict
derive instance genericVerdict :: Generic Verdict _
instance showVerdict :: Show Verdict where
  show = genericShow

-- | `self` is our client id, `ours` the version of our game and `theirs` the
-- | version the frame carries. Builds that predate versions send none; their
-- | frames are live plays, so we take them as a change we witnessed.
judge :: String -> Maybe Version -> Maybe Version -> Verdict
judge self ours = case _ of
  Nothing -> TakeTheirs $ tick self ours
  Just theirs
    | maybe true (_ < theirs) ours -> TakeTheirs theirs
    | ours == Just theirs -> AlreadyHave
    | otherwise -> SendOurs

-- | `^time:origin`, distinct from the mailbox's `@r` and `~r` stamps.
printVersion :: Version -> String
printVersion (Version time origin) = "^" <> show time <> ":" <> origin

parseVersion :: String -> Maybe Version
parseVersion s = do
  rest <- stripPrefix (Pattern "^") s
  colon <- indexOf (Pattern ":") rest
  let digits = CodeUnits.take colon rest
  guard $ digits /= "" && all isDigit (CodeUnits.toCharArray digits)
  time <- Int.fromString digits
  pure $ Version time $ CodeUnits.drop (colon + 1) rest
  where
    isDigit c = c >= '0' && c <= '9'

-- | Old builds read the whole segment as the sender id and never use it.
versionSender :: Version -> String -> String
versionSender version sender = sender <> "/" <> printVersion version

senderVersion :: String -> Maybe Version
senderVersion sender = lastIndexOf (Pattern "/^") sender
  >>= \i -> parseVersion (CodeUnits.drop (i + 1) sender)
