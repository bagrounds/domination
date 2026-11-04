module Test.LogicalClock (logical_clock_tests) where

import Prelude

import Data.Argonaut (decodeJson, encodeJson)
import Data.Array ((!!), (..), all, deleteAt, filter, foldl, init, length, snoc, take, updateAt)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Domination.Data.LogicalClock (Verdict(..), Version(..), judge, savedBeforeVersions, parseVersion, printVersion, senderVersion, tick, versionSender)
import Domination.Data.Mailbox (senderStamp)
import Domination.Data.Room (Admission(..), Header(..), Room(..), admission, headerSender, parseHeader, printHeader)
import Effect (Effect)
import Test.QuickCheck (Result(..), (<?>))
import Test.QuickCheck.Gen (Gen, chooseInt, elements, vectorOf)
import Test.Room (quick_check)

type EffectTest = { name :: String, test :: Effect Result }

logical_clock_tests :: Array EffectTest
logical_clock_tests =
  [ { name: "the old rule: a laptop returning with its saved game rewinds the phone"
    , test: pure $ case phoneAndLaptop Unversioned of
        { phone, laptop, phoneBefore } ->
          check (phone.game /= phoneBefore && phone.game `isPrefixOf` phoneBefore && laptop.game == phone.game)
            ("expected the old rule to rewind the phone; phone " <> show phone.game)
    }
  , { name: "versions: the phone keeps its game and the returning laptop catches up"
    , test: pure $ case phoneAndLaptop Versioned of
        { phone, laptop, phoneBefore } ->
          check (phone.game == phoneBefore && laptop.game == phoneBefore && laptop.version == phone.version)
            ("phone " <> show phone.game <> ", laptop " <> show laptop.game <> ", phone before " <> show phoneBefore)
    }
  , { name: "versions: a device that comes back ahead brings the other up to date"
    , test: pure $
        let
          w = story Versioned (devices 2)
            [ Open 0, Open 1, Move 0, Move 1, Close 0, Move 1, Move 1, Close 1, Open 0, Open 1 ]
          ahead = device 1 w
        in
          check ((device 0 w).game == ahead.game && length ahead.game == 4)
            ("phone " <> show (device 0 w).game <> ", laptop " <> show ahead.game)
    }
  , { name: "versions: an undo or a new game is a change, and everyone takes it"
    , test: pure $
        let
          w = story Versioned (devices 2)
            [ Open 0, Open 1, Move 0, Move 1, Move 0, Undo 1, Close 0, NewGame 1, Close 1, Open 0, Open 1 ]
        in
          check ((device 0 w).game == [ "new" ] && (device 1 w).game == [ "new" ])
            ("phone " <> show (device 0 w).game <> ", laptop " <> show (device 1 w).game)
    }
  , { name: "versions: nobody ever takes a game behind their own"
    , test: quick_check 500 do
        steps <- scheduleGen [ moveGen, closeGen, openGen, deliverGen, deliverGen ]
        let w = run Versioned (devices 3) steps
        pure $ isNothing w.rewound <?> (show steps <> " rewound: " <> show w.rewound)
    }
  , { name: "versions: once everyone is back, all devices hold the newest game"
    , test: quick_check 500 do
        steps <- scheduleGen [ moveGen, undoGen, newGameGen, closeGen, openGen, deliverGen, deliverGen ]
        let
          w = run Versioned (devices 3) steps
          newest = maximum $ _.version <$> w.devices
          back = quiesce $ foldl (flip step) w $ Open <$> (0 .. 2)
          agreed = all (\d -> d.game == (device 0 back).game && d.version == (device 0 back).version) back.devices
        pure $ (agreed && Just (device 0 back).version == newest)
          <?> (show steps <> " → " <> show (map (\d -> { g: d.game, v: d.version }) back.devices))
    }
  , { name: "versions never go backwards on any device"
    , test: quick_check 500 do
        steps <- scheduleGen [ moveGen, undoGen, newGameGen, closeGen, openGen, deliverGen ]
        let w = run Versioned (devices 3) steps
        pure $ w.monotonic <?> show steps
    }
  , { name: "games saved before versions: the one with more moves is ahead, and any change beats it"
    , test: quick_check 300 do
        a <- chooseInt 0 50
        b <- chooseInt 0 50
        origin <- originGen
        let
          older = savedBeforeVersions (min a b)
          newer = savedBeforeVersions (max a b)
          verdict = judge origin (Just older) (Just newer)
          expected = if a == b then AlreadyHave else TakeTheirs newer
        pure $ (verdict == expected && tick origin (Just newer) > newer
          && (origin == "" || Version (max a b) origin > newer))
          <?> show { a, b, verdict }
    }
  , { name: "a change is ahead of the game it changes"
    , test: quick_check 300 do
        current <- maybeVersionGen
        origin <- originGen
        pure $ (maybe true (_ < tick origin current) current) <?> show current
    }
  , { name: "judge takes only games ahead of ours and answers only games behind"
    , test: quick_check 500 do
        self <- originGen
        ours <- maybeVersionGen
        theirs <- maybeVersionGen
        pure $ case judge self ours theirs of
          TakeTheirs next -> maybe true (_ < next) ours <?> show { ours, theirs, next }
          SendOurs -> (((<) <$> theirs <*> ours) == Just true) <?> show { ours, theirs }
          AlreadyHave -> (ours == theirs && ours /= Nothing) <?> show { ours, theirs }
    }
  , { name: "two devices agree on which of their games is ahead"
    , test: quick_check 500 do
        a <- versionGen
        b <- versionGen
        let
          verdictA = judge "a" (Just a) (Just b)
          verdictB = judge "b" (Just b) (Just a)
        pure $ (case verdictA, verdictB of
          TakeTheirs _, SendOurs -> true
          SendOurs, TakeTheirs _ -> true
          AlreadyHave, AlreadyHave -> true
          _, _ -> false) <?> show { a, b, verdictA, verdictB }
    }
  , { name: "versions round-trip through the sender segment and storage"
    , test: quick_check 300 do
        version <- versionGen
        sender <- originGen
        pure $ (senderVersion (versionSender version sender) == Just version
          && parseVersion (printVersion version) == Just version
          && decodeJson (encodeJson version) == Right version)
          <?> show version
    }
  , { name: "versions and mailbox stamps never read as each other"
    , test: quick_check 300 do
        version <- versionGen
        sender <- originGen
        pure $ (isNothing (senderStamp (versionSender version sender))
          && all (isNothing <<< senderVersion) [ sender, sender <> "/@3", sender <> "/~3", sender <> "/^", sender <> "/^x:a", sender <> "/^:a" ])
          <?> show version
    }
  , { name: "versioned headers are admitted by the room parser old builds share"
    , test: quick_check 200 do
        version <- versionGen
        let header = printHeader (V1Header GlobalRoom (versionSender version "0b6e1f9a"))
        pure $ (admission GlobalRoom header == Admit
          && (senderVersion <<< headerSender <$> parseHeader header) == Right (Just version))
          <?> header
    }
  ]

check :: Boolean -> String -> Result
check true _ = Success
check false message = Failed message

-- A small world: devices that save their game, and a relay that forwards
-- frames only to devices that are online and may deliver them in any order.

data Policy = Versioned | Unversioned

type Device =
  { id :: String
  , online :: Boolean
  , game :: Array String
  , version :: Maybe Version
  , moves :: Int
  }

type Frame = { to :: Int, game :: Array String, version :: Maybe Version }

type World =
  { policy :: Policy
  , devices :: Array Device
  , inFlight :: Array Frame
  , rewound :: Maybe { device :: String, had :: Array String, took :: Array String }
  , monotonic :: Boolean
  }

data Step = Move Int | Undo Int | NewGame Int | Close Int | Open Int | Deliver Int

instance showStep :: Show Step where
  show = case _ of
    Move d -> "Move " <> show d
    Undo d -> "Undo " <> show d
    NewGame d -> "NewGame " <> show d
    Close d -> "Close " <> show d
    Open d -> "Open " <> show d
    Deliver k -> "Deliver " <> show k

devices :: Int -> World
devices n =
  { policy: Versioned
  , devices: (\i -> { id: "device" <> show i, online: false, game: [], version: Nothing, moves: 0 }) <$> (0 .. (n - 1))
  , inFlight: []
  , rewound: Nothing
  , monotonic: true
  }

device :: Int -> World -> Device
device i w = fromMaybe { id: "", online: false, game: [], version: Nothing, moves: 0 } $ w.devices !! i

-- Steps happen while frames are still on their way.
run :: Policy -> World -> Array Step -> World
run policy w0 = quiesce <<< foldl (flip step) w0 { policy = policy }

-- Each step's frames arrive before the next step: live play.
story :: Policy -> World -> Array Step -> World
story policy w0 = foldl (\w s -> quiesce $ step s w) w0 { policy = policy }

-- The phone and the laptop play together; the laptop's tab closes; the phone
-- plays on; the laptop comes back while the phone is open.
phoneAndLaptop :: Policy -> { phone :: Device, laptop :: Device, phoneBefore :: Array String }
phoneAndLaptop policy =
  let
    beforeReturn = story policy (devices 2)
      [ Open 0, Open 1, Move 0, Move 1, Close 1, Move 0, Move 0 ]
    after = story policy beforeReturn [ Open 1 ]
  in
    { phone: device 0 after, laptop: device 1 after, phoneBefore: (device 0 beforeReturn).game }

step :: Step -> World -> World
step s w = case s of
  Move d -> change d (\dev -> snoc dev.game (dev.id <> "#" <> show dev.moves)) w
  Undo d -> change d (\dev -> fromMaybe [] $ init dev.game) w
  NewGame d -> change d (const [ "new" ]) w
  Close d -> modifyDevice d _ { online = false } w
    # \w' -> w' { inFlight = filter (_.to >>> notEq d) w'.inFlight }
  Open d -> if (device d w).online then w else open d (modifyDevice d _ { online = true } w)
  Deliver k -> case w.inFlight !! k of
    Nothing -> w
    Just frame -> receive frame w { inFlight = fromMaybe w.inFlight $ deleteAt k w.inFlight }

-- A device changes its own game: a play, an undo or a new game.
change :: Int -> (Device -> Array String) -> World -> World
change d f w
  | not (device d w).online = w
  | otherwise =
      let
        dev = device d w
        next = case w.policy of
          Versioned -> Just $ tick dev.id dev.version
          Unversioned -> dev.version
        w' = modifyDevice d (_ { game = f dev, version = next, moves = dev.moves + 1 }) w
      in
        broadcast d w'

-- Coming back, a device offers its saved game and the others offer theirs.
-- The old rule only shared the saved game, and everyone took it.
open :: Int -> World -> World
open d w = case w.policy of
  Unversioned -> broadcast d w
  Versioned -> foldl (flip broadcast) w
    $ filter (\i -> (device i w).online && (device i w).version /= Nothing)
    $ 0 .. (length w.devices - 1)

broadcast :: Int -> World -> World
broadcast from w =
  let
    dev = device from w
    recipients = filter (\i -> i /= from && (device i w).online) $ 0 .. (length w.devices - 1)
  in
    w { inFlight = w.inFlight <> ((\to -> { to, game: dev.game, version: dev.version }) <$> recipients) }

receive :: Frame -> World -> World
receive frame w =
  let dev = device frame.to w
  in case w.policy of
    Unversioned -> takeFrame dev frame.version w
    Versioned -> case judge dev.id dev.version frame.version of
      TakeTheirs next -> takeFrame dev (Just next) w
      SendOurs -> broadcast frame.to w
      AlreadyHave -> w
  where
    -- Without undo, taking a strict prefix of our own game is a rewind.
    takeFrame dev next w' =
      let
        stale = frame.game /= dev.game && frame.game `isPrefixOf` dev.game
      in
        modifyDevice frame.to (_ { game = frame.game, version = next }) w'
          { rewound = if stale && isNothing w'.rewound
              then Just { device: dev.id, had: dev.game, took: frame.game }
              else w'.rewound
          , monotonic = w'.monotonic && next >= dev.version
          }

isPrefixOf :: forall a. Eq a => Array a -> Array a -> Boolean
isPrefixOf prefix xs = take (length prefix) xs == prefix

modifyDevice :: Int -> (Device -> Device) -> World -> World
modifyDevice i f w = w { devices = fromMaybe w.devices $ updateAt i (f $ device i w) w.devices }

quiesce :: World -> World
quiesce = go 2000
  where
    go fuel w
      | fuel <= 0 || length w.inFlight == 0 = w
      | otherwise = go (fuel - 1) (step (Deliver 0) w)

-- Generators

scheduleGen :: Array (Gen Step) -> Gen (Array Step)
scheduleGen gens = do
  n <- chooseInt 1 40
  body <- vectorOf n $ join $ elements $ fromMaybe (NEA.singleton moveGen) $ NEA.fromArray gens
  pure $ [ Open 0, Open 1, Open 2 ] <> body

deviceGen :: Gen Int
deviceGen = chooseInt 0 2

moveGen :: Gen Step
moveGen = Move <$> deviceGen

undoGen :: Gen Step
undoGen = Undo <$> deviceGen

newGameGen :: Gen Step
newGameGen = NewGame <$> deviceGen

closeGen :: Gen Step
closeGen = Close <$> deviceGen

openGen :: Gen Step
openGen = Open <$> deviceGen

deliverGen :: Gen Step
deliverGen = Deliver <$> chooseInt 0 6

originGen :: Gen String
originGen = elements $ NEA.cons' "0b6e1f9a-6c2d-4f6e-9b1a-2d3c4e5f6a7b"
  [ "x", "a-b-c", "phone", "laptop", "" ]

versionGen :: Gen Version
versionGen = Version <$> chooseInt 1 20 <*> originGen

maybeVersionGen :: Gen (Maybe Version)
maybeVersionGen = join $ elements $ NEA.cons' (pure Nothing) [ Just <$> versionGen ]
