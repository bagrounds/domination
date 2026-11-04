module Test.Resync (resync_tests) where

import Prelude

import Data.Array ((!!), (..), all, drop, elem, filter, foldl, length, mapWithIndex, nub, snoc, take, updateAt)
import Data.Array.NonEmpty as NEA
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), indexOf)
import Data.String.CodeUnits as CodeUnits
import Domination.Data.Backoff as Backoff
import Domination.Data.LogicalClock (Verdict(..), Version, judge, senderVersion, tick, versionSender)
import Domination.Data.Mailbox (Delivery(..), Dirty(..), PullOutcome(..), Pulled(..), Revision(..), Settlement(..), Stamp(..), Sync, deliveryStamp, initialSync, isClean, mailboxBackoff, nextRevision, ownChange, printRevisionTag, pullCursor, receiveFrame, senderStamp, settleDelivery, settlePull, stampSender)
import Domination.Data.Resync (Occasion(..), Reaction(..), Clock, allowedSilence, longestSilence, pollInterval, reactions, reopenAfterHidden)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Test.QuickCheck (Result(..), (<?>))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, resize)
import Test.Room (quick_check)

type EffectTest = { name :: String, test :: Effect Result }

-- A model of how clients keep a room's game in sync, faithful to Main and
-- src/FFI/WebSocket.js: the real pure decisions (Mailbox, LogicalClock,
-- Resync), sockets that can be live, silently dead (half-open) or closed,
-- sends queued while closed, a relay that only reaches live sockets, a
-- mailbox that can be down, retry backoff, and devices that sleep, wake,
-- close and reopen. One tick is one heartbeat (5 s).
--
-- The property users care about: once everyone is back, the network is
-- healthy and nobody moves, every device shows the latest game within a
-- short window, without anyone reloading.
resync_tests :: Array EffectTest
resync_tests = (scenarios >>= scenarioTests) <> chaosTests <> reactionTests

-- v0.40.0's policy: it looked again only on discrete events, never while a
-- page sat visible, and never doubted a socket the browser called open. The
-- scenarios below reproduce the reports against it.
legacy :: Policy
legacy = case _ of
  Started -> [ Announce, OfferGame, PullMailbox ]
  Shown _ -> [ PullMailbox ]
  NetworkBack -> [ PullMailbox ]
  RelayBack -> [ Announce, OfferGame, PullMailbox ]
  PeerJoined -> [ OfferGame ]
  RetryDue -> [ PullMailbox ]
  Ticked _ -> []

type Scenario = { name :: String, room :: Room, steps :: Array Step }

-- The wife is c0, the phone c1.
scenarios :: Array Scenario
scenarios = [ WithMailbox, WithoutMailbox ] >>= \room ->
  [ { room, name: "the phone that slept catches up with the moves made after it woke", steps: phoneSleeps }
  , { room, name: "the wife sees the moves the woken phone makes", steps: phoneMovesAfterWaking }
  , { room, name: "a short nap that killed the socket strands nobody", steps: phoneNaps }
  , { room, name: "a socket that dies while awake strands nobody", steps: deadWhileAwake }
  ]

scenarioTests :: Scenario -> Array EffectTest
scenarioTests { name: scenario, room, steps } =
  [ { name: scenario <> " (" <> show room <> ")"
    , test: pure $ scenarioResult $ play reactions room 2 steps
    }
  , { name: "v0.40.0 stranded a device: " <> scenario <> " (" <> show room <> ")"
    , test: pure $ strandedResult $ play legacy room 2 steps
    }
  ]

-- The phone sleeps with its socket left half-open, she plays, the phone
-- wakes (and looks once), she plays on.
phoneSleeps :: Array Step
phoneSleeps =
  [ Move 0, Deliver 0, Move 1, Deliver 0
  , Sleep 1 HalfOpen, Move 0, Tick 0, Tick 1
  , Wake 1 1800, Move 0, Move 0
  ]

phoneMovesAfterWaking :: Array Step
phoneMovesAfterWaking = [ Move 0, Deliver 0, Sleep 1 HalfOpen, Move 0, Wake 1 1800, Move 1 ]

phoneNaps :: Array Step
phoneNaps = [ Move 0, Deliver 0, Sleep 1 HalfOpen, Wake 1 10, Move 0 ]

deadWhileAwake :: Array Step
deadWhileAwake = [ Move 0, Deliver 0, Deliver 0, Deliver 0, Break 1 HalfOpen, Move 0 ]

play :: Policy -> Room -> Int -> Array Step -> World
play policy room n steps = calm policy (windowFor room) $ foldl (step policy) (start policy room n) steps

chaosTests :: Array EffectTest
chaosTests =
  [ { name: "mailbox rooms converge within " <> duration mailboxWindow <> " of everyone being back, whatever happened before"
    , test: quick_check 400 $ convergesAfterChaos WithMailbox
    }
  , { name: "rooms without a mailbox converge within " <> duration relayWindow <> " of everyone being back"
    , test: quick_check 400 $ convergesAfterChaos WithoutMailbox
    }
  , { name: "no device ever goes back to an older game (both rooms)"
    , test: quick_check 400 do
        room <- elements $ NEA.cons' WithMailbox [ WithoutMailbox ]
        n <- chooseInt 2 3
        steps <- stepsGen n
        let world = play reactions room n steps
        pure $ (world.violations == []) <?> show world.violations
    }
  ]

-- A visible page polls: within one poll a device that still holds a move
-- delivers it, and within the next everyone else sees it.
mailboxWindow :: Int
mailboxWindow = 2 * ticksFor pollInterval + 1

-- Without a mailbox only the relay helps, and a dead socket is noticed
-- within the longest silence.
relayWindow :: Int
relayWindow = ticksFor longestSilence + 1

ticksFor :: Milliseconds -> Int
ticksFor (Milliseconds wait) = ceil (wait / toNumber tickMs)

duration :: Int -> String
duration ticks = show (ticks * tickMs / 1000) <> " s"

windowFor :: Room -> Int
windowFor = case _ of
  WithMailbox -> mailboxWindow
  WithoutMailbox -> relayWindow

convergesAfterChaos :: Room -> Gen Result
convergesAfterChaos room = do
  n <- chooseInt 2 3
  steps <- stepsGen n
  let world = play reactions room n steps
  pure $ (world.violations == [] && converged world)
    <?> show steps <> "\n" <> describe world

scenarioResult :: World -> Result
scenarioResult world
  | world.violations == [] && converged world = Success
  | otherwise = Failed $ describe world

strandedResult :: World -> Result
strandedResult world
  | world.violations == [] && not (converged world) = Success
  | otherwise = Failed $ "expected a safe but stranded room: " <> describe world

reactionTests :: Array EffectTest
reactionTests =
  [ { name: "a hidden page does nothing on a heartbeat"
    , test: quick_check 200 do
        clock <- clockGen
        let r = reactions (Ticked clock { visible = false })
        pure $ (r == []) <?> show r
    }
  , { name: "a visible page pulls on a heartbeat exactly when its last pull is a poll interval old"
    , test: quick_check 200 do
        clock <- clockGen
        let pulls = elem PullMailbox (reactions (Ticked clock { visible = true }))
        pure $ (pulls == (clock.sincePull >= pollInterval)) <?> show clock
    }
  , { name: "a visible page reopens an open socket exactly when the relay has been silent too long"
    , test: quick_check 200 do
        clock <- clockGen
        let reopens = elem ReopenRelay (reactions (Ticked clock { visible = true, relayOpen = true }))
        pure $ (reopens == (clock.sinceHeard >= allowedSilence clock.silentReopens)) <?> show clock
    }
  , { name: "a page leaves a socket it knows is closed to the socket's own reconnect backoff"
    , test: quick_check 200 do
        clock <- clockGen
        let r = reactions (Ticked clock { relayOpen = false })
        pure $ not (elem ReopenRelay r) <?> show clock
    }
  , { name: "each silent reopen waits longer, never beyond the longest silence"
    , test: quick_check 200 do
        n <- chooseInt 0 40
        pure $ (allowedSilence n <= allowedSilence (n + 1) && allowedSilence (n + 1) <= longestSilence)
          <?> show n
    }
  , { name: "a page shown after a long nap pulls and reopens its socket; after a short one it only pulls"
    , test: quick_check 200 do
        away <- millisecondsGen
        let
          expected = if away >= reopenAfterHidden then [ PullMailbox, ReopenRelay ] else [ PullMailbox ]
          actual = reactions (Shown away)
        pure $ (actual == expected) <?> show away <> " " <> show actual
    }
  ]

clockGen :: Gen Clock
clockGen = do
  visible <- elements $ NEA.cons' true [ false ]
  relayOpen <- elements $ NEA.cons' true [ false ]
  sincePull <- millisecondsGen
  sinceHeard <- millisecondsGen
  silentReopens <- chooseInt 0 8
  pure { visible, relayOpen, sincePull, sinceHeard, silentReopens }

millisecondsGen :: Gen Milliseconds
millisecondsGen = Milliseconds <<< toNumber <<< (_ * 1000) <$> chooseInt 0 300

-- The model

data Room = WithMailbox | WithoutMailbox

derive instance eqRoom :: Eq Room
instance showRoom :: Show Room where
  show = case _ of
    WithMailbox -> "mailbox room"
    WithoutMailbox -> "no mailbox"

data Link = Live | HalfOpen | Down

derive instance eqLink :: Eq Link
instance showLink :: Show Link where
  show = case _ of
    Live -> "Live"
    HalfOpen -> "HalfOpen"
    Down -> "Down"

data Kind = GameFrame | JoinFrame | HeartbeatFrame

derive instance eqKind :: Eq Kind

type Outgoing = { kind :: Kind, envelope :: String }

type Frame = { to :: Int, epoch :: Int, kind :: Kind, envelope :: String }

type Device =
  { open :: Boolean
  , visible :: Boolean
  , link :: Link
  , epoch :: Int
  , queue :: Array Outgoing
  , game :: Maybe String
  , sync :: Sync
  , version :: Maybe Version
  , moves :: Int
  , adoptions :: Int
  , unreachable :: Boolean
  , failures :: Int
  , retryIn :: Maybe Int
  , sincePull :: Int
  , sinceHeard :: Int
  , silentReopens :: Int
  }

type World =
  { room :: Room
  , server :: { revision :: Revision, envelope :: Maybe String }
  , mailboxUp :: Boolean
  , devices :: Array Device
  , frames :: Array Frame
  , fresh :: Int
  , violations :: Array String
  }

type Policy = Occasion -> Array Reaction

data Step
  = Move Int
  | Sleep Int Link
  | Wake Int Int
  | Break Int Link
  | Reconnect Int
  | Deliver Int
  | Tick Int
  | Close Int
  | Open Int
  | Outage
  | Restore

instance showStep :: Show Step where
  show = case _ of
    Move d -> "Move " <> show d
    Sleep d l -> "Sleep " <> show d <> " " <> show l
    Wake d s -> "Wake " <> show d <> " " <> show s
    Break d l -> "Break " <> show d <> " " <> show l
    Reconnect d -> "Reconnect " <> show d
    Deliver i -> "Deliver " <> show i
    Tick d -> "Tick " <> show d
    Close d -> "Close " <> show d
    Open d -> "Open " <> show d
    Outage -> "Outage"
    Restore -> "Restore"

tickMs :: Int
tickMs = 5000

ms :: Int -> Milliseconds
ms ticks = Milliseconds $ toNumber $ ticks * tickMs

name :: Int -> String
name d = "c" <> show d

session :: Device -> Device
session x = x
  { visible = true, link = Live, epoch = x.epoch + 1, queue = []
  , moves = 0, adoptions = 0, unreachable = false, failures = 0, retryIn = Nothing
  , sincePull = 0, sinceHeard = 0, silentReopens = 0
  }

newDevice :: Device
newDevice = session
  { open: true, visible: true, link: Live, epoch: 0, queue: []
  , game: Nothing, sync: initialSync, version: Nothing
  , moves: 0, adoptions: 0, unreachable: false, failures: 0, retryIn: Nothing
  , sincePull: 0, sinceHeard: 0, silentReopens: 0
  }

start :: Policy -> Room -> Int -> World
start policy room n = foldl (\w d -> occur policy d Started w) world (0 .. (n - 1))
  where
    world =
      { room
      , server: { revision: Revision 0, envelope: Nothing }
      , mailboxUp: true
      , devices: map (const newDevice) (1 .. n)
      , frames: []
      , fresh: 0
      , violations: []
      }

device :: Int -> World -> Device
device d world = fromMaybe newDevice (world.devices !! d)

modify :: Int -> (Device -> Device) -> World -> World
modify d f world = checked d (device d world) next
  where
    next = world { devices = fromMaybe world.devices (updateAt d (f (device d world)) world.devices) }

-- Safety, checked on every change: nobody goes back to an older game.
checked :: Int -> Device -> World -> World
checked d before world = world { violations = world.violations <> problems }
  where
    after = device d world
    problems = filter (_ /= "")
      [ if after.sync.confirmed < before.sync.confirmed then name d <> " went back to an older revision" else ""
      , if after.sync.confirmed > world.server.revision then name d <> " is ahead of the mailbox" else ""
      , if after.version < before.version then name d <> " went back to an older version" else ""
      ]

step :: Policy -> World -> Step -> World
step policy world = case _ of
  Move d -> whenOpen d (move policy d) world
  Sleep d link -> whenOpen d (modify d \x -> x { visible = false, link = sleepLink link x.link }) world
  Wake d seconds -> whenOpen d (wake policy d seconds) world
  Break d link -> whenOpen d (breakLink d link) world
  Reconnect d -> whenOpen d (reconnect policy d) world
  Deliver i -> deliverFrame policy i world
  Tick d -> whenOpen d (tickDevice policy d) world
  Close d -> whenOpen d (modify d _ { open = false, visible = false, link = Down, queue = [] }) world
  Open d
    | (device d world).open -> world
    | otherwise -> occur policy d Started $ modify d (session >>> _ { open = true }) world
  Outage -> world { mailboxUp = false }
  Restore -> world { mailboxUp = true }
  where
    sleepLink wanted current = case wanted, current of
      _, Down -> Down
      Live, _ -> current
      _, _ -> wanted

whenOpen :: Int -> (World -> World) -> World -> World
whenOpen d f world = if (device d world).open then f world else world

wake :: Policy -> Int -> Int -> World -> World
wake policy d seconds world
  | (device d world).visible = world
  | otherwise = occur policy d (Shown (Milliseconds (toNumber seconds * 1000.0)))
      $ modify d _ { visible = true } world

breakLink :: Int -> Link -> World -> World
breakLink d link world = case (device d world).link, link of
  Live, HalfOpen -> modify d _ { link = HalfOpen } world
  Down, _ -> world
  _, Down -> modify d _ { link = Down } world
  _, _ -> world

-- The socket's own reconnect timer (FFI): only after a close it noticed.
reconnect :: Policy -> Int -> World -> World
reconnect policy d world
  | (device d world).link == Down = occur policy d RelayBack $ flush d
      $ modify d (\x -> x { link = Live, epoch = x.epoch + 1 }) world
  | otherwise = world

reopen :: Policy -> Int -> World -> World
reopen policy d world = occur policy d RelayBack $ flush d
  $ modify d (\x -> x
      { link = Live, epoch = x.epoch + 1, sinceHeard = 0, silentReopens = x.silentReopens + 1 })
  world

flush :: Int -> World -> World
flush d world = foldl (\w { kind, envelope } -> broadcast d kind envelope w)
  (modify d _ { queue = [] } world)
  (device d world).queue

occur :: Policy -> Int -> Occasion -> World -> World
occur policy d occasion world = foldl (flip $ perform policy d) world (policy occasion)

perform :: Policy -> Int -> Reaction -> World -> World
perform policy d = case _ of
  Announce -> broadcast d JoinFrame (name d <> "|join")
  OfferGame -> offer d
  PullMailbox -> pull policy d
  ReopenRelay -> reopen policy d

broadcast :: Int -> Kind -> String -> World -> World
broadcast d kind envelope world = case (device d world).link of
  Live -> world { frames = world.frames <> others }
  HalfOpen -> world
  Down -> modify d (\x -> x { queue = takeEnd 100 (snoc x.queue { kind, envelope }) }) world
  where
    others = map (\o -> { to: o, epoch: (device o world).epoch, kind, envelope })
      $ filter (\o -> o /= d && (device o world).link /= Down) (0 .. (length world.devices - 1))
    takeEnd k xs = drop (length xs - k) xs

offer :: Int -> World -> World
offer d world = case world.room, x.game, x.version of
  WithoutMailbox, Just game, Just version -> broadcast d GameFrame (versionSender version (name d) <> "|" <> game) world
  _, _, _ -> world
  where
    x = device d world

move :: Policy -> Int -> World -> World
move policy d world
  | not (device d world).visible = world
  | otherwise = case world.room of
      WithMailbox -> deliver policy d 8 $ modify d
        (\x -> x { game = Just game, moves = x.moves + 1, sync = ownChange x.sync }) fresh
      WithoutMailbox -> offer d $ modify d
        (\x -> x { game = Just game, version = Just (tick (name d) x.version) }) fresh
  where
    game = "g" <> show world.fresh
    fresh = world { fresh = world.fresh + 1 }

adopt :: String -> Device -> Device
adopt envelope x = x { game = Just (gameOf envelope), adoptions = x.adoptions + 1 }

gameOf :: String -> String
gameOf envelope = maybe envelope (\i -> CodeUnits.drop (i + 1) envelope) (indexOf (Pattern "|") envelope)

senderOf :: String -> String
senderOf envelope = maybe envelope (\i -> CodeUnits.take i envelope) (indexOf (Pattern "|") envelope)

deliverFrame :: Policy -> Int -> World -> World
deliverFrame policy pick world = case world.frames !! index of
  Nothing -> world
  Just frame -> receive policy frame world { frames = take index world.frames <> drop (index + 1) world.frames }
  where
    index = if length world.frames == 0 then 0 else pick `mod` length world.frames

receive :: Policy -> Frame -> World -> World
receive policy { to, epoch, kind, envelope } world =
  if x.open && x.link == Live && x.epoch == epoch
  then case kind of
    JoinFrame -> occur policy to PeerJoined heard
    HeartbeatFrame -> heard
    GameFrame -> receiveGame policy to envelope heard
  else world
  where
    x = device to world
    heard = modify to _ { sinceHeard = 0, silentReopens = 0 } world

receiveGame :: Policy -> Int -> String -> World -> World
receiveGame policy d envelope world = case world.room of
  WithMailbox -> case receiveFrame (senderStamp (senderOf envelope)) x.sync of
    Nothing -> world
    Just next ->
      let adopted = modify d (adopt envelope >>> _ { sync = next }) world
      in if isClean next then adopted else deliver policy d 8 adopted
  WithoutMailbox -> case judge (name d) x.version (senderVersion (senderOf envelope)) of
    TakeTheirs version -> modify d (adopt envelope >>> _ { version = Just version }) world
    SendOurs -> offer d world
    AlreadyHave -> world
  where
    x = device d world

markReachable :: Int -> World -> World
markReachable d = modify d _ { failures = 0, unreachable = false }

scheduleRetry :: Int -> World -> World
scheduleRetry d = modify d \x -> case x.retryIn of
  Just _ -> x { unreachable = true }
  Nothing ->
    let
      failures = x.failures + 1
      Milliseconds wait = Backoff.delay mailboxBackoff failures 0.5
    in x { unreachable = true, failures = failures, retryIn = Just (max 1 (ceil (wait / toNumber tickMs))) }

pull :: Policy -> Int -> World -> World
pull policy d world0 = case world.room of
  WithoutMailbox -> world0
  WithMailbox
    | not (isClean x.sync) -> deliver policy d 8 world
    | not world.mailboxUp -> scheduleRetry d world
    | otherwise -> case settlePull (isJust x.game) x.sync pulled of
        UpToDate -> markReachable d world
        CatchUp { sync, envelope } -> modify d (adopt envelope >>> _ { sync = sync }) $ markReachable d world
        Upload sync -> deliver policy d 8 $ modify d _ { sync = sync } $ markReachable d world
        PullRetry _ -> scheduleRetry d world
  where
    world = modify d _ { sincePull = 0 } world0
    x = device d world
    pulled
      | pullCursor x.sync == Just (printRevisionTag world.server.revision) = Unchanged
      | otherwise = Latest world.server.revision world.server.envelope

-- One PUT and its reply, then whatever Main does next (`continueDelivery`).
deliver :: Policy -> Int -> Int -> World -> World
deliver _ _ 0 world = world
deliver policy d fuel world = case x.game of
  Just game | not (isClean x.sync) ->
    let
      envelope = stampSender (deliveryStamp x.sync) (name d) <> "|" <> game
      base = x.sync.confirmed
      sent = { base, envelope, moves: x.moves, adoptions: x.adoptions }
      broadcastFirst = x.unreachable && x.sync.dirty == Moved
      tentative = stampSender (Tentative base) (name d) <> "|" <> game
      before = if broadcastFirst then broadcast d GameFrame tentative world else world
      stored = world.server.revision == base
      delivery
        | not world.mailboxUp = Unreachable "down"
        | stored = Stored (nextRevision base)
        | otherwise = Conflict world.server.revision world.server.envelope
      afterPut
        | world.mailboxUp && stored = before { server = { revision: nextRevision base, envelope: Just envelope } }
        | otherwise = before
      mine = case delivery of
        Conflict r _ -> Just $ stampSender (Committed r) (name d) <> "|" <> game
        _ -> Nothing
      again = continue
    in case settleDelivery sent { moves: x.moves, adoptions: x.adoptions } mine x.sync delivery of
      Stale -> again $ markReachable d afterPut
      Accepted { sync, broadcast: frame, replay } -> again
        $ broadcast d GameFrame frame
        $ modify d (\y -> (if replay then adopt frame y else y) { sync = sync })
        $ markReachable d afterPut
      Adopt { sync, envelope: theirs } -> again
        $ modify d (adopt theirs >>> _ { sync = sync })
        $ markReachable d afterPut
      Redeliver sync -> again $ modify d _ { sync = sync } $ markReachable d afterPut
      Retry { sync, broadcastTentative } -> scheduleRetry d
        $ (if broadcastTentative && not broadcastFirst
            then broadcast d GameFrame (stampSender (Tentative sync.confirmed) (name d) <> "|" <> game)
            else identity)
        $ modify d _ { sync = sync } afterPut
  _ -> world
  where
    x = device d world
    continue w = if isClean (device d w).sync then w else deliver policy d (fuel - 1) w

tickDevice :: Policy -> Int -> World -> World
tickDevice policy d world0 = occur policy d (Ticked clock) $ heartbeat $ retry counted
  where
    counted = modify d (\x -> x { sincePull = x.sincePull + 1, sinceHeard = x.sinceHeard + 1 }) world0
    retry w = case (device d w).retryIn of
      Just n | n <= 1 -> occur policy d RetryDue $ modify d _ { retryIn = Nothing } w
      Just n -> modify d _ { retryIn = Just (n - 1) } w
      Nothing -> w
    heartbeat w = if (device d w).link /= Down then broadcast d HeartbeatFrame (name d <> "|beat") w else w
    x' = device d (heartbeat (retry counted))
    clock =
      { visible: x'.visible
      , relayOpen: x'.link /= Down
      , sincePull: ms x'.sincePull
      , sinceHeard: ms x'.sinceHeard
      , silentReopens: x'.silentReopens
      }

-- Everyone is back: tabs reopened, phones awake, closed sockets reconnected,
-- the mailbox up, and nobody moves. Sockets that died silently stay dead
-- unless a client notices. Then `window` heartbeats pass.
calm :: Policy -> Int -> World -> World
calm policy window world0 = drain policy $ foldl (\w _ -> beat (drain policy w)) back (1 .. window)
  where
    indices = 0 .. (length world0.devices - 1)
    back = foldl (\w d -> reconnect policy d $ wake policy d 600 $ step policy w (Open d))
      world0 { mailboxUp = true } indices
    beat w = foldl (\w' d -> tickDevice policy d w') w indices

drain :: Policy -> World -> World
drain policy = go 2000
  where
    go 0 world = world
    go n world = case world.frames of
      [] -> world
      _ -> go (n - 1) (deliverFrame policy 0 world)

converged :: World -> Boolean
converged world = case world.room of
  WithMailbox -> all agrees world.devices
  WithoutMailbox -> length (nub (map (\x -> { game: x.game, version: x.version }) world.devices)) == 1
  where
    agrees x = isClean x.sync
      && x.sync.confirmed == world.server.revision
      && x.game == map gameOf world.server.envelope

describe :: World -> String
describe world = "mailbox " <> show world.server.revision <> " " <> show (gameOf <$> world.server.envelope)
  <> " violations " <> show world.violations
  <> " devices " <> show (mapWithIndex showDevice world.devices)
  where
    showDevice i x = name i <> ":" <> show x.game <> "/" <> show x.sync.confirmed <> "/" <> show x.sync.dirty
      <> "/" <> show x.version <> "/" <> show x.link

stepsGen :: Int -> Gen (Array Step)
stepsGen n = resize 40 $ arrayOf do
  d <- chooseInt 0 (n - 1)
  pick <- chooseInt 0 30
  seconds <- elements $ NEA.cons' 2 [ 30, 1800 ]
  link <- elements $ NEA.cons' Live [ HalfOpen, HalfOpen, Down ]
  elements $ NEA.cons' (Move d)
    [ Move d, Move d, Sleep d link, Wake d seconds, Break d link, Reconnect d
    , Deliver pick, Deliver pick, Deliver pick, Tick d, Tick d
    , Close d, Open d, Outage, Restore
    ]
