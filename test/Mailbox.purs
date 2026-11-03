module Test.Mailbox (mailbox_tests) where

import Prelude

import Data.Argonaut (decodeJson, encodeJson)
import Data.Array ((!!), (..), all, deleteAt, filter, foldl, length, mapWithIndex, updateAt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (Pattern(..), indexOf)
import Data.String.CodeUnits as CodeUnits
import Domination.Data.Mailbox (Delivery(..), Dirty(..), PullOutcome(..), Pulled(..), Revision(..), Sent, Settlement(..), Stamp(..), Sync, deliveryStamp, initialSync, interpretDelivery, interpretPull, isClean, mailboxRoomUrl, nextRevision, ownChange, overtakes, parseRevisionTag, parseStamp, printRevisionTag, printStamp, pullCursor, receiveFrame, senderStamp, settleDelivery, settlePull, stampSender)
import Domination.Data.Room (Admission(..), Header(..), Room(..), admission, headerSender, parseHeader, printHeader, roomCode)
import Effect (Effect)
import Test.QuickCheck (Result(..), (<?>))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, vectorOf)
import Data.Array.NonEmpty as NEA
import Test.Room (quick_check)

type EffectTest = { name :: String, test :: Effect Result }

mailbox_tests :: Array EffectTest
mailbox_tests =
  [ { name: "revision tags round-trip"
    , test: quick_check 200 do
        r <- revisionGen
        pure $ (parseRevisionTag (printRevisionTag r) == Just r) <?> show r
    }
  , { name: "malformed revision tags are rejected"
    , test: pure $ check
        (all (isNothing <<< parseRevisionTag) [ "", "5", "\"-1\"", "\"1.5\"", "W/\"3\"", "\"\"", "*" ])
        "a malformed tag parsed"
    }
  , { name: "stamps round-trip through the sender segment"
    , test: quick_check 200 do
        stamp <- stampGen
        sender <- elements $ NEA.cons' "0b6e1f9a-6c2d-4f6e-9b1a-2d3c4e5f6a7b" [ "x", "a-b-c" ]
        pure $ (senderStamp (stampSender stamp sender) == Just stamp
          && parseStamp (printStamp stamp) == Just stamp)
          <?> show stamp
    }
  , { name: "senders from old builds carry no stamp"
    , test: pure $ check
        (all (isNothing <<< senderStamp)
          [ "0b6e1f9a-6c2d-4f6e-9b1a-2d3c4e5f6a7b", "abc", "a/b", "a/@", "a/@x", "a/~-1" ])
        "an unstamped sender produced a stamp"
    }
  , { name: "stamped headers are admitted by the room parser old builds share"
    , test: quick_check 200 do
        stamp <- stampGen
        pure $ case roomCode "abc" of
          Nothing -> Failed "room code"
          Just code ->
            let header = printHeader (V1Header (GameRoom code) (stampSender stamp "0b6e1f9a"))
            in (admission (GameRoom code) header == Admit
              && (senderStamp <<< headerSender <$> parseHeader header) == Right (Just stamp))
              <?> header
    }
  , { name: "a live frame never lowers confirmed"
    , test: quick_check 500 do
        sync <- syncGen
        stamp <- maybeStampGen
        pure $ maybe true (\next -> next.confirmed >= sync.confirmed) (receiveFrame stamp sync)
          <?> show stamp <> " lowered " <> show sync.confirmed
    }
  , { name: "committed frames are adopted once, in revision order"
    , test: quick_check 500 do
        sync <- syncGen
        r <- revisionGen
        let result = receiveFrame (Just $ Committed r) sync
        pure $ (if r > sync.confirmed
          then result == Just { confirmed: r, dirty: Clean, uncertain: [] }
          else isNothing result)
          <?> show r <> " vs " <> show sync.confirmed
    }
  , { name: "tentative frames make a courier unless overtaken"
    , test: quick_check 500 do
        sync <- syncGen
        b <- revisionGen
        let result = receiveFrame (Just $ Tentative b) sync
        pure $ (if b >= sync.confirmed
          then (_.dirty <$> result) == Just Relaying && (_.confirmed <$> result) == Just b
          else isNothing result)
          <?> show b <> " vs " <> show sync.confirmed
    }
  , { name: "unstamped frames are only relayed before the first commit"
    , test: pure $ check
        ((_.dirty <$> receiveFrame Nothing initialSync) == Just Relaying
          && (_.dirty <$> receiveFrame Nothing initialSync { confirmed = Revision 3 }) == Just Clean)
        "unexpected unstamped frame handling"
    }
  , { name: "an adopted competitor overtakes our unsent move"
    , test: pure $ check
        (overtakes (ownChange initialSync) { confirmed: Revision 1, dirty: Clean, uncertain: [] }
          && not (overtakes initialSync { confirmed: Revision 1, dirty: Clean, uncertain: [] })
          && not (overtakes (ownChange initialSync) { confirmed: Revision 2, dirty: Clean, uncertain: [] }))
        "overtakes"
    }
  , { name: "deliveries are stamped as the next revision"
    , test: quick_check 200 do
        sync <- syncGen
        pure $ (deliveryStamp sync == Committed (nextRevision sync.confirmed)) <?> show sync.confirmed
    }
  , { name: "HTTP replies are interpreted"
    , test: pure $ check
        (interpretDelivery { status: 200, etag: Just "\"4\"", body: "" } == Stored (Revision 4)
          && interpretDelivery { status: 412, etag: Just "\"4\"", body: "e" } == Conflict (Revision 4) (Just "e")
          && interpretDelivery { status: 412, etag: Just "\"4\"", body: "" } == Conflict (Revision 4) Nothing
          && isUnreachable (interpretDelivery { status: 0, etag: Nothing, body: "Failed to fetch" })
          && isUnreachable (interpretDelivery { status: 500, etag: Nothing, body: "" })
          && isUnreachable (interpretDelivery { status: 200, etag: Nothing, body: "" })
          && interpretPull { status: 304, etag: Just "\"4\"", body: "" } == Unchanged
          && interpretPull { status: 200, etag: Just "\"4\"", body: "e" } == Latest (Revision 4) (Just "e")
          && interpretPull { status: 204, etag: Just "\"0\"", body: "" } == Latest (Revision 0) Nothing
          && interpretPull { status: 200, etag: Just "W/\"4\"", body: "e" } == Latest (Revision 4) (Just "e"))
        "unexpected interpretation"
    }
  , { name: "pulls have no cursor before the first commit"
    , test: pure $ check
        (pullCursor initialSync == Nothing
          && pullCursor initialSync { confirmed = Revision 7 } == Just "\"7\"")
        "pullCursor"
    }
  , { name: "a pull never moves a client backwards"
    , test: quick_check 500 do
        sync <- syncGen
        r <- revisionGen
        body <- elements $ NEA.cons' Nothing [ Just "e" ]
        hasGame <- elements $ NEA.cons' true [ false ]
        pure $ case settlePull hasGame sync (Latest r body) of
          CatchUp { sync: next } -> (next.confirmed > sync.confirmed) <?> "caught up backwards"
          Upload next -> (hasGame && body == Nothing && next.dirty == Moved) <?> "uploaded without a game"
          _ -> Success
    }
  , { name: "a delivery reply never lowers confirmed unless the mailbox was reset"
    , test: quick_check 1000 do
        sync <- syncGen
        sent <- sentGen sync
        delivery <- deliveryGen
        now <- { moves: _, adoptions: _ } <$> chooseInt 0 2 <*> chooseInt 0 2
        let
          settled = settleDelivery sent now Nothing sync delivery
          reset = case delivery of
            Conflict r _ -> r < sent.base
            _ -> false
        pure $ (reset || confirmedOf sync settled >= sync.confirmed)
          <?> show delivery <> " lowered " <> show sync.confirmed <> " to " <> show (confirmedOf sync settled)
    }
  , { name: "an unreachable mailbox keeps the envelope as uncertain"
    , test: quick_check 200 do
        sync <- syncGen
        sent <- sentGen sync
        pure $ case settleDelivery sent { moves: sent.moves, adoptions: sent.adoptions } Nothing sync (Unreachable "x") of
          Retry { sync: next } -> (next.uncertain !! 0 == Just sent.envelope && length next.uncertain <= 8)
            <?> "uncertain not recorded"
          other -> Failed $ show other
    }
  , { name: "a 412 holding our own uncertain write commits it"
    , test: pure $
        let
          sync = { confirmed: Revision 2, dirty: Moved, uncertain: [ "mine@3" ] }
          sent = { base: Revision 2, envelope: "mine@3", moves: 1, adoptions: 0 }
        in case settleDelivery sent { moves: 1, adoptions: 0 } (Just "mine@3") sync (Conflict (Revision 3) (Just "mine@3")) of
          Accepted { sync: next } -> check (next == { confirmed: Revision 3, dirty: Clean, uncertain: [] }) (show next)
          other -> Failed $ show other
    }
  , { name: "sync state survives JSON"
    , test: quick_check 200 do
        sync <- syncGen
        pure $ (decodeJson (encodeJson sync) == Right sync) <?> show sync.confirmed
    }
  , { name: "mailbox URLs name the room"
    , test: pure $ check
        (map (mailboxRoomUrl "https://m.example/") (roomCode "abc") == Just "https://m.example/v1/rooms/abc"
          && map (mailboxRoomUrl "https://m.example") (roomCode "abc") == Just "https://m.example/v1/rooms/abc")
        "mailboxRoomUrl"
    }
  , { name: "simulated rooms converge on the mailbox despite lost frames and replies"
    , test: quick_check 300 do
        steps <- arrayOf stepGen
        let
          world = foldl step (newWorld 3) steps
          settled = quiesce world
        pure $ (world.ok && settled.ok && converged settled)
          <?> describe settled
    }
  ]

check :: Boolean -> String -> Result
check true _ = Success
check false message = Failed message

isUnreachable :: Delivery -> Boolean
isUnreachable = case _ of
  Unreachable _ -> true
  _ -> false

confirmedOf :: Sync -> Settlement -> Revision
confirmedOf sync = case _ of
  Stale -> sync.confirmed
  Accepted { sync: s } -> s.confirmed
  Adopt { sync: s } -> s.confirmed
  Redeliver s -> s.confirmed
  Retry { sync: s } -> s.confirmed

revisionGen :: Gen Revision
revisionGen = Revision <$> chooseInt 0 20

stampGen :: Gen Stamp
stampGen = do
  r <- revisionGen
  elements $ NEA.cons' (Committed r) [ Tentative r ]

maybeStampGen :: Gen (Maybe Stamp)
maybeStampGen = do
  stamp <- stampGen
  elements $ NEA.cons' Nothing [ Just stamp, Just stamp ]

syncGen :: Gen Sync
syncGen = do
  confirmed <- revisionGen
  dirty <- elements $ NEA.cons' Clean [ Moved, Relaying ]
  uncertainCount <- chooseInt 0 3
  uncertain <- vectorOf uncertainCount (elements $ NEA.cons' "a" [ "b", "c" ])
  pure { confirmed, dirty, uncertain }

sentGen :: Sync -> Gen Sent
sentGen sync = do
  back <- chooseInt 0 2
  envelope <- elements $ NEA.cons' "a" [ "b", "c", "d" ]
  let Revision c = sync.confirmed
  pure { base: Revision (max 0 (c - back)), envelope, moves: 0, adoptions: 0 }

deliveryGen :: Gen Delivery
deliveryGen = do
  r <- revisionGen
  body <- elements $ NEA.cons' Nothing [ Just "a", Just "b", Just "z" ]
  elements $ NEA.cons' (Stored r) [ Conflict r body, Unreachable "down" ]

-- A small world: a compare-and-set server, clients running the pure protocol,
-- and a relay that loses and reorders frames.

type Server = { revision :: Revision, envelope :: Maybe String }

type Client =
  { sync :: Sync
  , game :: Maybe String
  , moves :: Int
  , adoptions :: Int
  , pending :: Maybe { sent :: Sent, delivery :: Delivery }
  }

type Frame = { to :: Int, envelope :: String }

type World =
  { server :: Server
  , clients :: Array Client
  , frames :: Array Frame
  , fresh :: Int
  , ok :: Boolean
  }

data Step
  = Move Int
  | Send Int Loss
  | Reply Int
  | DeliverFrame Int Boolean
  | Pull Int

data Loss = Delivered | LostRequest | LostReply

stepGen :: Gen Step
stepGen = do
  client <- chooseInt 0 2
  pick <- chooseInt 0 20
  drop <- elements $ NEA.cons' false [ false, true ]
  loss <- elements $ NEA.cons' Delivered [ Delivered, Delivered, LostRequest, LostReply ]
  elements $ NEA.cons'
    (Move client)
    [ Move client, Send client loss, Reply client, Reply client
    , DeliverFrame pick drop, DeliverFrame pick false, Pull client
    ]

newWorld :: Int -> World
newWorld n =
  { server: { revision: Revision 0, envelope: Nothing }
  , clients: map (const newClient) (1 .. n)
  , frames: []
  , fresh: 0
  , ok: true
  }

newClient :: Client
newClient = { sync: initialSync, game: Nothing, moves: 0, adoptions: 0, pending: Nothing }

encode :: Int -> Stamp -> String -> String
encode i stamp game = stampSender stamp ("c" <> show i) <> "|" <> game

gameOf :: String -> String
gameOf envelope = maybe envelope (\i -> CodeUnits.drop (i + 1) envelope) (indexOf (Pattern "|") envelope)

stampOf :: String -> Maybe Stamp
stampOf envelope = maybe Nothing (\i -> senderStamp (CodeUnits.take i envelope)) (indexOf (Pattern "|") envelope)

clientAt :: World -> Int -> Client
clientAt world i = fromMaybe newClient (world.clients !! i)

withClient :: Int -> Client -> World -> World
withClient i client world =
  let next = world { clients = fromMaybe world.clients (updateAt i client world.clients) }
      before = clientAt world i
  in next { ok = next.ok && client.sync.confirmed >= before.sync.confirmed && client.sync.confirmed <= next.server.revision }

broadcastFrom :: Int -> String -> World -> World
broadcastFrom i envelope world = world
  { frames = world.frames <> map (\to -> { to, envelope })
      (filter (_ /= i) (0 .. (length world.clients - 1)))
  }

adopt :: String -> Client -> Client
adopt envelope client = client { game = Just (gameOf envelope), adoptions = client.adoptions + 1 }

put :: String -> String -> Server -> { delivery :: Delivery, server :: Server }
put ifMatch body server
  | parseRevisionTag ifMatch == Just server.revision =
      let revision = nextRevision server.revision
      in { delivery: Stored revision, server: { revision, envelope: Just body } }
  | otherwise = { delivery: Conflict server.revision server.envelope, server }

send :: Loss -> Int -> World -> World
send loss i world =
  let client = clientAt world i
  in case client.game, client.pending of
    Just game, Nothing | not (isClean client.sync) ->
      let
        envelope = encode i (deliveryStamp client.sync) game
        sent = { base: client.sync.confirmed, envelope, moves: client.moves, adoptions: client.adoptions }
        answered = put (printRevisionTag sent.base) envelope world.server
        server = case loss of
          LostRequest -> world.server
          _ -> answered.server
        delivery = case loss of
          Delivered -> answered.delivery
          _ -> Unreachable "lost"
      in withClient i client { pending = Just { sent, delivery } } world { server = server }
    _, _ -> world

reply :: Int -> World -> World
reply i world =
  let client = clientAt world i
  in case client.pending of
    Nothing -> world
    Just { sent, delivery } ->
      let
        mine = case delivery of
          Conflict r _ -> encode i (Committed r) <$> client.game
          _ -> Nothing
        settled = settleDelivery sent { moves: client.moves, adoptions: client.adoptions } mine client.sync delivery
        idle = client { pending = Nothing }
      in case settled of
        Stale -> continue i $ withClient i idle world
        Accepted { sync, broadcast, replay } ->
          let replayed = if replay then adopt broadcast idle else idle
          in continue i $ broadcastFrom i broadcast $ withClient i replayed { sync = sync } world
        Adopt { sync, envelope } ->
          continue i $ withClient i (adopt envelope idle) { sync = sync } world
        Redeliver sync -> continue i $ withClient i idle { sync = sync } world
        Retry { sync, broadcastTentative } ->
          let
            tentative = case broadcastTentative, idle.game of
              true, Just game -> broadcastFrom i (encode i (Tentative sync.confirmed) game)
              _, _ -> identity
          in tentative $ withClient i idle { sync = sync } world

continue :: Int -> World -> World
continue = send Delivered

step :: World -> Step -> World
step world = case _ of
  Move i ->
    let client = clientAt world i
    in send Delivered i $ withClient i client
      { game = Just ("g" <> show world.fresh)
      , sync = ownChange client.sync
      , moves = client.moves + 1
      } world { fresh = world.fresh + 1 }
  Send i loss -> send loss i world
  Reply i -> reply i world
  DeliverFrame pick drop -> deliverFrame pick drop world
  Pull i -> pull i world

deliverFrame :: Int -> Boolean -> World -> World
deliverFrame pick drop world = case world.frames !! index of
  Nothing -> world
  Just { to, envelope } ->
    let rest = world { frames = fromMaybe world.frames (deleteAt index world.frames) }
    in if drop then rest else receive to envelope rest
  where
    index = if length world.frames == 0 then 0 else pick `mod` length world.frames

receive :: Int -> String -> World -> World
receive i envelope world =
  let client = clientAt world i
  in case receiveFrame (stampOf envelope) client.sync of
    Nothing -> world
    Just sync -> continue i $ withClient i (adopt envelope client) { sync = sync } world

pull :: Int -> World -> World
pull i world =
  let client = clientAt world i
  in case client.pending of
    Just _ -> world
    Nothing
      | not (isClean client.sync) -> continue i world
      | otherwise ->
          let
            pulled =
              if pullCursor client.sync == Just (printRevisionTag world.server.revision)
              then Unchanged
              else Latest world.server.revision world.server.envelope
          in case settlePull (isJust client.game) client.sync pulled of
            CatchUp { sync, envelope } -> withClient i (adopt envelope client) { sync = sync } world
            Upload sync -> continue i $ withClient i client { sync = sync } world
            _ -> world

-- Everything comes back online: replies arrive, frames are delivered, and
-- every client pulls, until nothing changes.
quiesce :: World -> World
quiesce = go 20
  where
    go 0 world = world
    go n world = go (n - 1) $ pullAll $ drain $ replyAll world
    indices world = 0 .. (length world.clients - 1)
    replyAll world = foldl (\w i -> reply i (send Delivered i w)) world (indices world)
    drain world = case world.frames of
      [] -> world
      _ -> drain $ replyAll $ deliverFrame 0 false world
    pullAll world = foldl (\w i -> replyAll (pull i w)) world (indices world)

converged :: World -> Boolean
converged world = all agrees world.clients
  where
    agrees client = isClean client.sync
      && client.sync.confirmed == world.server.revision
      && client.game == map gameOf world.server.envelope
      && isNothing client.pending

describe :: World -> String
describe world = "server " <> show world.server.revision <> " " <> show world.server.envelope
  <> " ok " <> show world.ok
  <> " clients " <> show (mapWithIndex (\i c -> show i <> ":" <> show c.sync.confirmed <> "/" <> show c.sync.dirty <> "/" <> show c.game) world.clients)
