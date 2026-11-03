--| The room mailbox's client protocol (design §6.4), as pure functions.
--|
--| The mailbox keeps the latest game envelope of each room under a revision
--| number that the server assigns. Each client remembers the highest revision
--| it knows was committed (`confirmed`) and whether its local game is that
--| committed state (`dirty`). Everything here decides; `Main` only performs
--| the HTTP requests and replays envelopes.
module Domination.Data.Mailbox
  ( Revision(..)
  , nextRevision
  , printRevisionTag
  , parseRevisionTag
  , Stamp(..)
  , printStamp
  , parseStamp
  , stampSender
  , senderStamp
  , Dirty(..)
  , Sync
  , initialSync
  , maxUncertain
  , isClean
  , receiveFrame
  , overtakes
  , ownChange
  , deliveryStamp
  , HttpResponse
  , Delivery(..)
  , interpretDelivery
  , Pulled(..)
  , interpretPull
  , pullCursor
  , Sent
  , Progress
  , Settlement(..)
  , settleDelivery
  , PullOutcome(..)
  , settlePull
  , Status(..)
  , status
  , mailboxRoomUrl
  , mailboxBackoff
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Array (all, elem, filter, take, (:))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), lastIndexOf, stripPrefix, stripSuffix)
import Data.String.CodeUnits as CodeUnits
import Domination.Data.Backoff (Backoff)
import Domination.Data.Room (RoomCode, unRoomCode)
import Effect.Aff (Milliseconds(..))

newtype Revision = Revision Int

derive instance eqRevision :: Eq Revision
derive instance ordRevision :: Ord Revision
derive newtype instance showRevision :: Show Revision
derive newtype instance encodeJsonRevision :: EncodeJson Revision
derive newtype instance decodeJsonRevision :: DecodeJson Revision

nextRevision :: Revision -> Revision
nextRevision (Revision r) = Revision (r + 1)

-- | HTTP entity tags carry the revision: `"5"`.
printRevisionTag :: Revision -> String
printRevisionTag (Revision r) = "\"" <> show r <> "\""

parseRevisionTag :: String -> Maybe Revision
parseRevisionTag = stripPrefix (Pattern "\"")
  >=> stripSuffix (Pattern "\"")
  >=> parseRevision

parseRevision :: String -> Maybe Revision
parseRevision digits = Revision <$> (guard isDecimal *> Int.fromString digits)
  where
    isDecimal = digits /= "" && all isDigit (CodeUnits.toCharArray digits)
    isDigit c = c >= '0' && c <= '9'

-- | Appended to the sender segment of a game envelope's header (§6.4.3).
-- | `Committed r`: this game is revision `r`. `Tentative b`: not committed,
-- | built on revision `b`.
data Stamp
  = Committed Revision
  | Tentative Revision

derive instance eqStamp :: Eq Stamp
derive instance genericStamp :: Generic Stamp _
instance showStamp :: Show Stamp where
  show = genericShow

printStamp :: Stamp -> String
printStamp = case _ of
  Committed (Revision r) -> "@" <> show r
  Tentative (Revision r) -> "~" <> show r

parseStamp :: String -> Maybe Stamp
parseStamp s =
  (Committed <$> (stripPrefix (Pattern "@") s >>= parseRevision))
  <|> (Tentative <$> (stripPrefix (Pattern "~") s >>= parseRevision))

-- | Old builds read the whole segment as the sender id and never use it.
stampSender :: Stamp -> String -> String
stampSender stamp sender = sender <> "/" <> printStamp stamp

senderStamp :: String -> Maybe Stamp
senderStamp sender = lastIndexOf (Pattern "/") sender
  >>= \i -> parseStamp (CodeUnits.drop (i + 1) sender)

-- | `Clean`: the local game is the committed state at `confirmed`.
-- | `Moved`: the local game has our own changes on top of it.
-- | `Relaying`: the local game came from someone else and is not committed.
data Dirty = Clean | Moved | Relaying

derive instance eqDirty :: Eq Dirty
derive instance genericDirty :: Generic Dirty _
instance showDirty :: Show Dirty where
  show = genericShow

instance encodeJsonDirty :: EncodeJson Dirty where
  encodeJson = encodeJson <<< show

instance decodeJsonDirty :: DecodeJson Dirty where
  decodeJson json = decodeJson json >>= case _ of
    "Clean" -> Right Clean
    "Moved" -> Right Moved
    "Relaying" -> Right Relaying
    other -> Left $ UnexpectedValue $ encodeJson other

-- | `uncertain` holds envelopes we sent but never heard back about: any of
-- | them may have been stored.
type Sync =
  { confirmed :: Revision
  , dirty :: Dirty
  , uncertain :: Array String
  }

initialSync :: Sync
initialSync = { confirmed: Revision 0, dirty: Clean, uncertain: [] }

maxUncertain :: Int
maxUncertain = 8

isClean :: Sync -> Boolean
isClean = _.dirty >>> eq Clean

commit :: Revision -> Sync
commit confirmed = { confirmed, dirty: Clean, uncertain: [] }

-- | A game frame from the live relay. `Nothing` means ignore it.
receiveFrame :: Maybe Stamp -> Sync -> Maybe Sync
receiveFrame stamp sync@{ confirmed } = case stamp of
  Just (Committed r)
    | r > confirmed -> Just $ commit r
    | otherwise -> Nothing
  Just (Tentative b)
    | b > confirmed -> Just { confirmed: b, dirty: Relaying, uncertain: [] }
    | b == confirmed -> Just sync { dirty = Relaying }
    | otherwise -> Nothing
  Nothing
    | confirmed == Revision 0 -> Just sync { dirty = Relaying }
    | otherwise -> Just sync { dirty = Clean }

-- | Adopting `next` replaced a move of ours that a direct competitor beat.
overtakes :: Sync -> Sync -> Boolean
overtakes previous next = previous.dirty == Moved
  && next.dirty == Clean
  && next.confirmed == nextRevision previous.confirmed

ownChange :: Sync -> Sync
ownChange = _ { dirty = Moved }

deliveryStamp :: Sync -> Stamp
deliveryStamp = Committed <<< nextRevision <<< _.confirmed

-- | What `fetch` saw. Status 0 means no response at all.
type HttpResponse = { status :: Int, etag :: Maybe String, body :: String }

data Delivery
  = Stored Revision
  | Conflict Revision (Maybe String)
  | Unreachable String

derive instance eqDelivery :: Eq Delivery
derive instance genericDelivery :: Generic Delivery _
instance showDelivery :: Show Delivery where
  show = genericShow

revisionOf :: HttpResponse -> Maybe Revision
revisionOf = _.etag >=> parseRevisionTag

nonEmpty :: String -> Maybe String
nonEmpty = case _ of
  "" -> Nothing
  s -> Just s

interpretDelivery :: HttpResponse -> Delivery
interpretDelivery response = case response.status, revisionOf response of
  200, Just r -> Stored r
  412, Just r -> Conflict r (nonEmpty response.body)
  s, _ -> Unreachable $ "PUT answered " <> show s

data Pulled
  = Unchanged
  | Latest Revision (Maybe String)
  | PullFailed String

derive instance eqPulled :: Eq Pulled
derive instance genericPulled :: Generic Pulled _
instance showPulled :: Show Pulled where
  show = genericShow

interpretPull :: HttpResponse -> Pulled
interpretPull response = case response.status, revisionOf response of
  304, _ -> Unchanged
  200, Just r -> Latest r (nonEmpty response.body)
  204, Just r -> Latest r Nothing
  s, _ -> PullFailed $ "GET answered " <> show s

-- | `If-None-Match` for a pull. Before the first commit there is no cursor,
-- | so an empty room answers `204` and a saved game gets delivered.
pullCursor :: Sync -> Maybe String
pullCursor { confirmed }
  | confirmed == Revision 0 = Nothing
  | otherwise = Just $ printRevisionTag confirmed

-- | A delivery in flight: the revision it was based on, the envelope, and
-- | counters to tell what happened locally while it was in flight.
type Sent =
  { base :: Revision
  , envelope :: String
  , moves :: Int
  , adoptions :: Int
  }

type Progress = { moves :: Int, adoptions :: Int }

data Settlement
  = Stale
  | Accepted
    { sync :: Sync
    , broadcast :: String
    , replay :: Boolean
    , notApplied :: Boolean
    }
  | Adopt { sync :: Sync, envelope :: String, notApplied :: Boolean }
  | Redeliver Sync
  | Retry { sync :: Sync, broadcastTentative :: Boolean }

derive instance eqSettlement :: Eq Settlement
derive instance genericSettlement :: Generic Settlement _
instance showSettlement :: Show Settlement where
  show = genericShow

-- | `mine` is the current local game encoded as the revision a `412` reports,
-- | to tell whether an uncertain write the server holds is still our game.
settleDelivery
  :: Sent
  -> Progress
  -> Maybe String
  -> Sync
  -> Delivery
  -> Settlement
settleDelivery sent now mine sync = case _ of
  Stored r
    | r <= sync.confirmed -> Stale
    | adopted -> Accepted
        { sync: commit r
        , broadcast: sent.envelope
        , replay: true
        , notApplied: moved
        }
    | moved -> Accepted
        { sync: { confirmed: r, dirty: Moved, uncertain: [] }
        , broadcast: sent.envelope
        , replay: false
        , notApplied: false
        }
    | otherwise -> Accepted
        { sync: commit r
        , broadcast: sent.envelope
        , replay: false
        , notApplied: false
        }
  Conflict r body
    | r < sent.base -> Redeliver sync { confirmed = r, uncertain = [] }
    | r <= sync.confirmed -> Stale
    | otherwise -> case body of
        Nothing -> Redeliver sync { confirmed = r, uncertain = [] }
        Just envelope
          | envelope `elem` sync.uncertain ->
              if mine == Just envelope
              then Accepted
                { sync: commit r
                , broadcast: envelope
                , replay: false
                , notApplied: false
                }
              else Redeliver sync { confirmed = r, uncertain = [] }
          | otherwise -> Adopt
              { sync: commit r
              , envelope
              , notApplied: sync.dirty == Moved && r == nextRevision sync.confirmed
              }
  Unreachable _ -> Retry
    { sync: sync
      { uncertain = take maxUncertain
          $ sent.envelope : filter (_ /= sent.envelope) sync.uncertain
      }
    , broadcastTentative: sync.dirty == Moved
        && not (sent.envelope `elem` sync.uncertain)
    }
  where
    adopted = now.adoptions /= sent.adoptions
    moved = now.moves /= sent.moves

data PullOutcome
  = UpToDate
  | CatchUp { sync :: Sync, envelope :: String }
  | Upload Sync
  | PullRetry String

derive instance eqPullOutcome :: Eq PullOutcome
derive instance genericPullOutcome :: Generic PullOutcome _
instance showPullOutcome :: Show PullOutcome where
  show = genericShow

-- | `hasGame`: a game is saved locally and can be delivered.
settlePull :: Boolean -> Sync -> Pulled -> PullOutcome
settlePull hasGame sync = case _ of
  Unchanged -> UpToDate
  Latest r (Just envelope)
    | r > sync.confirmed -> CatchUp { sync: commit r, envelope }
    | otherwise -> UpToDate
  Latest r Nothing
    | hasGame -> Upload { confirmed: r, dirty: Moved, uncertain: [] }
    | otherwise -> UpToDate
  PullFailed e -> PullRetry e

-- | For the small mark next to the connection count.
data Status = Synced | Pending | Offline

derive instance eqStatus :: Eq Status
derive instance genericStatus :: Generic Status _
instance showStatus :: Show Status where
  show = genericShow

status :: { sync :: Sync, unreachable :: Boolean } -> Status
status { sync, unreachable }
  | unreachable = Offline
  | isClean sync = Synced
  | otherwise = Pending

mailboxRoomUrl :: String -> RoomCode -> String
mailboxRoomUrl base code = withoutTrailingSlash base <> "/v1/rooms/" <> unRoomCode code
  where
    withoutTrailingSlash url = fromMaybe url $ stripSuffix (Pattern "/") url

mailboxBackoff :: Backoff
mailboxBackoff = { base: Milliseconds 5000.0, cap: Milliseconds 300000.0 }
