module Test.Storage (storage_panel_tests) where

import Prelude

import Data.Array (all, any, filter, length, nub, sort)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), contains)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Domination.Data.Room (Room(..), RoomCode, roomCode, scopedKey, unRoomCode)
import Domination.Data.StorageInventory (Context, Entry, Group(..), Item, Locator(..), Owner(..), Freshness(..), Selection(..), WebStorage(..), classify, formatCount, groupOf, inventory, isDeletable, item, removalPlan, roughShare, storageSummary, summaryOf)
import Domination.Data.StorageKey (DeviceKey(..), RoomKey(..), StorageKey(..), allDeviceKeys, describe, isHeldInMemory, parseKey, printKey)
import Domination.Data.Undo (historySlots)
import Domination.UI.Storage (confirmation)
import Effect (Effect)
import Test.QuickCheck (Result, assertEquals, (<?>))
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, oneOf, vectorOf)
import Test.Room (all_success, quick_check)

type EffectTest = { name :: String, test :: Effect Result }

storage_panel_tests :: Array EffectTest
storage_panel_tests =
  key_tests
    <> classification_tests
    <> inventory_tests
    <> plan_tests
    <> presentation_tests

-- ── Generators ──

letters :: NEA.NonEmptyArray Char
letters = NEA.cons' 'a' $ toCharArray "bcdefghijklmnopqrstuvwxyz"

codeChars :: NEA.NonEmptyArray Char
codeChars = letters <> NEA.cons' '0' (toCharArray "123456789")

stringOf :: Int -> Int -> NEA.NonEmptyArray Char -> Gen String
stringOf minLength maxLength chars = do
  n <- chooseInt minLength maxLength
  fromCharArray <$> vectorOf n (elements chars)

codeGen :: Gen RoomCode
codeGen = do
  raw <- stringOf 1 12 codeChars
  maybe codeGen pure $ roomCode raw

-- | Codes of letters only, long enough not to appear in a sentence by chance.
distinctiveCodeGen :: Gen RoomCode
distinctiveCodeGen = do
  raw <- stringOf 8 12 letters
  maybe distinctiveCodeGen pure $ roomCode raw

roomGen :: Gen Room
roomGen = oneOf $ NEA.cons' (pure GlobalRoom) [ GameRoom <$> codeGen ]

roomKeyGen :: Gen RoomKey
roomKeyGen = oneOf $ NEA.cons' (pure CurrentGame)
  [ UndoStep <$> chooseInt 0 (historySlots - 1)
  , pure Chat
  , pure MailboxSync
  , pure GameVersion
  ]

deviceKeyGen :: Gen DeviceKey
deviceKeyGen = elements $ NEA.cons' PlayerId allDeviceKeys

storageKeyGen :: Gen StorageKey
storageKeyGen = oneOf $ NEA.cons' (InRoom <$> roomGen <*> roomKeyGen) [ OnDevice <$> deviceKeyGen ]

-- | Names no version of Domination writes, with or without a room prefix.
strangeNameGen :: Gen String
strangeNameGen = ("x-" <> _) <$> stringOf 1 10 codeChars

sizeGen :: Gen (Maybe Int)
sizeGen = oneOf $ NEA.cons' (Just <$> chooseInt 0 100000) [ pure Nothing ]

versionGen :: Gen String
versionGen = (\a b c -> "v" <> show a <> "." <> show b <> "." <> show c)
  <$> chooseInt 0 2 <*> chooseInt 0 60 <*> chooseInt 0 9

scope :: String
scope = "https://domination.fun/"

contextGen :: Gen Context
contextGen = { room: _, version: _, scope } <$> roomGen <*> versionGen

locatorGen :: Context -> Gen Locator
locatorGen ctx = oneOf $ NEA.cons'
  (WebStorageKey Local <<< printKey <$> storageKeyGen)
  [ WebStorageKey Local <<< printKey <<< InRoom ctx.room <$> roomKeyGen
  , WebStorageKey Local <$> (scopedKey <$> roomGen <*> strangeNameGen)
  , WebStorageKey Local <$> strangeNameGen
  , WebStorageKey Session <$> strangeNameGen
  , (\name version -> Database { name, version }) <$> strangeNameGen <*> chooseInt 1 5
  , Cache <$> oneOf (NEA.cons' versionGen [ pure ctx.version, strangeNameGen ])
  , (\s -> Registration { scope: s, script: s <> "sw.js" }) <$> oneOf (NEA.cons' (pure scope) [ ("https://other.example/" <> _) <$> strangeNameGen ])
  , OriginFile <$> strangeNameGen
  , Cookie <$> strangeNameGen
  ]

-- | Entries as a browser lists them: each locator at most once.
entriesGen :: Context -> Gen (Array Entry)
entriesGen ctx = Array.nubByEq (\a b -> a.locator == b.locator)
  <$> arrayOf ({ locator: _, size: _ } <$> locatorGen ctx <*> sizeGen)

scenarioGen :: Gen { ctx :: Context, items :: Array Item }
scenarioGen = do
  ctx <- contextGen
  entries <- entriesGen ctx
  pure { ctx, items: item ctx <$> entries }

-- ── Keys ──

key_tests :: Array EffectTest
key_tests =
  [ { name: "∀ key: parseKey undoes printKey"
    , test: quick_check 500 do
        key <- storageKeyGen
        pure $ parseKey (printKey key) == Just key <?> show key
    }
  , { name: "∀ keys: different keys print differently"
    , test: quick_check 500 do
        a <- storageKeyGen
        b <- storageKeyGen
        pure $ (printKey a /= printKey b || a == b) <?> show a <> " and " <> show b
    }
  , { name: "∀ name not printed by printKey: parseKey rejects it"
    , test: quick_check 300 do
        room <- roomGen
        name <- strangeNameGen
        pure $ parseKey (scopedKey room name) == Nothing <?> name
    }
  , { name: "undo steps outside the history, or with leading zeros, aren't keys"
    , test: pure $ assertEquals
        (parseKey <$> [ "game_state_10", "game_state_01", "game_state_-1", "room/abc/game_state_9" ])
        [ Nothing, Nothing, Nothing, Just $ InRoom (GameRoom code) (UndoStep 9) ]
    }
  , { name: "every device key has its own name, label and consequence"
    , test: pure $ all_success
        [ assertEquals (length $ nub $ printKey <<< OnDevice <$> allDeviceKeys) (length allDeviceKeys)
        , assertEquals (length $ nub $ _.label <<< describe <<< OnDevice <$> allDeviceKeys) (length allDeviceKeys)
        , all (\key -> (describe (OnDevice key)).consequence /= "") allDeviceKeys <?> "a consequence is empty"
        ]
    }
  , { name: "diagnostics, the old seed and the clear counter aren't held in memory; preferences are"
    , test: pure $ assertEquals
        (isHeldInMemory <<< OnDevice <$> [ ConsoleLog, SyncJournal, LegacyRelaySeed, ClearGeneration, Username, Kingdom ])
        [ false, false, false, false, true, true ]
    }
  ]
  where
    code = unsafeCode "abc"

unsafeCode :: String -> RoomCode
unsafeCode name = case roomCode name of
  Just code -> code
  Nothing -> unsafeCode "a"

-- ── Classification ──

classification_tests :: Array EffectTest
classification_tests =
  [ { name: "∀ room code c and unknown name n: c's prefix puts n in room c and no other"
    , test: quick_check 300 do
        ctx <- contextGen
        code <- codeGen
        name <- strangeNameGen
        let owner = classify ctx $ WebStorageKey Local $ scopedKey (GameRoom code) name
        pure $ owner == RoomData code name <?> show owner
    }
  , { name: "∀ codes c and c<>s: a key of room c<>s is never counted in room c"
    , test: quick_check 300 do
        code <- codeGen
        suffix <- stringOf 1 3 codeChars
        key <- roomKeyGen
        let
          longer = unsafeCode $ unRoomCode code <> suffix
          ctx = { room: GameRoom code, version: "v0.0.0", scope }
          locator = WebStorageKey Local $ printKey $ InRoom (GameRoom longer) key
          i = { locator, size: Just 1, owner: classify ctx locator }
        pure $ groupOf ctx i == OtherRoom longer <?> show (groupOf ctx i)
    }
  , { name: "∀ bare unknown name: it is not recognized"
    , test: quick_check 300 do
        ctx <- contextGen
        name <- strangeNameGen
        pure $ classify ctx (WebStorageKey Local name) == Unrecognized <?> name
    }
  , { name: "a cache named after the running version is current; other versions are stale; other names aren't Domination's"
    , test: pure $ assertEquals
        (classify { room: GlobalRoom, version: "v0.46.0", scope } <<< Cache <$> [ "v0.46.0", "v0.45.1", "v1.2", "images" ])
        [ AppShell Current, AppShell Stale, Unrecognized, Unrecognized ]
    }
  , { name: "only the service worker at the page's scope is Domination's"
    , test: pure $ assertEquals
        (classify { room: GlobalRoom, version: "v0.46.0", scope } <<< Registration <$>
          [ { scope, script: scope <> "sw.js" }, { scope: "https://domination.fun/other/", script: "" } ])
        [ AppShell Current, Unrecognized ]
    }
  , { name: "session storage is never taken for Domination's own keys"
    , test: pure $ assertEquals
        (classify { room: GlobalRoom, version: "v0.46.0", scope } $ WebStorageKey Session "username")
        Unrecognized
    }
  ]

-- ── Inventory ──

inventory_tests :: Array EffectTest
inventory_tests =
  [ { name: "∀ entries: every entry is in exactly one section"
    , test: quick_check 200 do
        { ctx, items } <- scenarioGen
        let sections = inventory ctx (entryOf <$> items)
        pure $ sort (show <<< _.locator <$> (sections >>= _.items)) == sort (show <<< _.locator <$> items)
          <?> show (length items)
    }
  , { name: "∀ entries: the sections' summaries add up to the summary of everything"
    , test: quick_check 200 do
        { ctx, items } <- scenarioGen
        let sections = inventory ctx (entryOf <$> items)
        pure $ fold (_.summary <$> sections) == summaryOf items <?> show (length items)
    }
  , { name: "∀ entries: each section holds only its own group, once"
    , test: quick_check 200 do
        { ctx, items } <- scenarioGen
        let
          sections = inventory ctx (entryOf <$> items)
          groups = _.group <$> sections
        pure $ (length (nub groups) == length groups
            && all (\{ group, items: own } -> all ((_ == group) <<< groupOf ctx) own) sections)
          <?> show groups
    }
  ]

entryOf :: Item -> Entry
entryOf { locator, size } = { locator, size }

-- ── Removal plans ──

plan_tests :: Array EffectTest
plan_tests =
  [ { name: "∀ room: clearing it removes exactly its keys"
    , test: quick_check 200 do
        { ctx, items } <- scenarioGen
        fresh <- codeGen
        code <- elements $ NEA.cons' fresh $ Array.mapMaybe roomOf items
        let
          group = if GameRoom code == ctx.room then ThisRoom else OtherRoom code
          plan = removalPlan ctx items (WholeGroup group)
          inRoom i = roomOf i == Just code
        pure $ map (show <<< _.locator) plan.items == map (show <<< _.locator) (filter inRoom items)
          <?> show code
    }
  , { name: "∀ entries: clearing all Domination data removes only names Domination writes"
    , test: quick_check 200 do
        { ctx, items } <- scenarioGen
        let plan = removalPlan ctx items AllDomination
        pure $ all (isKnown <<< _.owner) plan.items <?> show (_.locator <$> plan.items)
    }
  , { name: "∀ selection: no plan removes the clear counter, cookies, or the running app's files (except Reinstall)"
    , test: quick_check 200 do
        { ctx, items } <- scenarioGen
        selection <- selectionGen ctx items
        let plan = removalPlan ctx items selection
        pure $ (selection == ReinstallAppFiles || all isDeletable plan.items) <?> show selection
    }
  , { name: "∀ selection: a plan advances the clear counter exactly when it removes a key pages hold"
    , test: quick_check 300 do
        { ctx, items } <- scenarioGen
        selection <- selectionGen ctx items
        let plan = removalPlan ctx items selection
        pure $ plan.advancesGeneration == any (maybe false isHeldInMemory <<< knownKey) plan.items
          <?> show selection
    }
  , { name: "∀ selection but Reinstall: this page reloads exactly when it holds a removed key"
    , test: quick_check 300 do
        { ctx, items } <- scenarioGen
        selection <- selectionGen ctx items
        let
          plan = removalPlan ctx items selection
          heldHere key = isHeldInMemory key && case key of
            InRoom room _ -> room == ctx.room
            OnDevice _ -> true
        pure $ (selection == ReinstallAppFiles
            || plan.reloadsThisPage == any (maybe false heldHere <<< knownKey) plan.items)
          <?> show selection
    }
  , { name: "∀ entries: Reinstall removes Domination's app files and reloads"
    , test: quick_check 200 do
        { ctx, items } <- scenarioGen
        let plan = removalPlan ctx items ReinstallAppFiles
        pure $ (plan.reloadsThisPage && all (isAppShell <<< _.owner) plan.items
            && length plan.items == length (filter (isAppShell <<< _.owner) items))
          <?> show (_.locator <$> plan.items)
    }
  , { name: "deleting another room's game doesn't reload this page; deleting this room's does"
    , test: pure $ assertEquals
        ((\room -> (removalPlan globalContext (gameIn room) (OneItem $ locator room)).reloadsThisPage) <$> [ GameRoom other, GlobalRoom ])
        [ false, true ]
    }
  ]
  where
    other = unsafeCode "other"
    globalContext = { room: GlobalRoom, version: "v0.46.0", scope }
    locator room = WebStorageKey Local $ printKey $ InRoom room CurrentGame
    gameIn room = [ item globalContext { locator: locator room, size: Just 10 } ]

selectionGen :: Context -> Array Item -> Gen Selection
selectionGen ctx items = oneOf $ NEA.cons' (pure AllDomination)
  [ pure ReinstallAppFiles
  , WholeGroup <<< groupOf ctx <$> elements (NEA.cons' fallback items)
  , OneItem <<< _.locator <$> elements (NEA.cons' fallback items)
  ]
  where
    fallback = item ctx { locator: WebStorageKey Local "username", size: Just 1 }

knownKey :: Item -> Maybe StorageKey
knownKey { owner } = case owner of
  Known key -> Just key
  _ -> Nothing

roomOf :: Item -> Maybe RoomCode
roomOf { owner } = case owner of
  Known (InRoom (GameRoom code) _) -> Just code
  RoomData code _ -> Just code
  _ -> Nothing

isKnown :: Owner -> Boolean
isKnown = case _ of
  Known _ -> true
  _ -> false

isAppShell :: Owner -> Boolean
isAppShell = case _ of
  AppShell _ -> true
  _ -> false

-- ── Presentation ──

presentation_tests :: Array EffectTest
presentation_tests =
  [ { name: "∀ n ≥ 0: formatCount groups digits by three and loses none"
    , test: quick_check 300 do
        n <- chooseInt 0 2000000000
        let formatted = formatCount n
        pure $ (String.replaceAll (Pattern ",") (String.Replacement "") formatted == show n
            && all (\group -> String.length group == 3) (Array.drop 1 $ String.split (Pattern ",") formatted))
          <?> formatted
    }
  , { name: "formatCount examples"
    , test: pure $ assertEquals (formatCount <$> [ 0, 999, 1000, 1284113, -1234 ]) [ "0", "999", "1,000", "1,284,113", "-1,234" ]
    }
  , { name: "roughShare examples"
    , test: pure $ assertEquals
        (roughShare 100.0 <$> [ 0.0, 0.2, 25.0, 99.6, 150.0 ])
        [ "none", "under 1%", "about 25%", "all", "all" ]
    }
  , { name: "∀ rooms: the bug report's storage line names no room"
    , test: quick_check 200 do
        codes <- arrayOf distinctiveCodeGen
        let
          entries = (\c -> { locator: WebStorageKey Local $ printKey $ InRoom (GameRoom c) CurrentGame, size: Just 5 }) <$> codes
          summary = storageSummary { room: GlobalRoom, version: "v0.46.0", scope } entries
        pure $ not (any (\c -> contains (Pattern $ unRoomCode c) summary) codes) <?> summary
    }
  , { name: "the bug report's storage line counts rooms and sizes by category"
    , test: pure $ assertEquals
        (storageSummary { room: GlobalRoom, version: "v0.46.0", scope }
          [ { locator: WebStorageKey Local "room/abc/game_state", size: Just 1000 }
          , { locator: WebStorageKey Local "room/abc/chat", size: Just 500 }
          , { locator: WebStorageKey Local "room/xyz/chat", size: Just 20 }
          , { locator: WebStorageKey Local "game_state", size: Just 7 }
          , { locator: WebStorageKey Local "username", size: Just 3 }
          , { locator: WebStorageKey Local "debug_log", size: Just 2000 }
          , { locator: WebStorageKey Local "mystery", size: Just 1 }
          , { locator: WebStorageKey Session "elsewhere", size: Just 99 }
          ])
        "3,531 chars in 7 keys; 2 rooms (largest 1,500); global room 7; preferences 3; diagnostics 2,000; not recognized 1"
    }
  , { name: "the bug report's storage line counts one of a kind in the singular"
    , test: pure $ assertEquals
        (storageSummary { room: GlobalRoom, version: "v0.46.0", scope }
          [ { locator: WebStorageKey Local "room/abc/chat", size: Just 1 } ])
        "1 char in 1 key; 1 room (largest 1); global room 0; preferences 0; diagnostics 0; not recognized 0"
    }
  , { name: "∀ single deletion: the confirmation states its consequence, and the reload when there is one"
    , test: quick_check 200 do
        { ctx, items } <- scenarioGen
        selection <- OneItem <<< _.locator <$> elements (NEA.cons' (item ctx { locator: WebStorageKey Local "kingdom", size: Just 4 }) items)
        let
          plan = removalPlan ctx items selection
          lines = confirmation { origin: "https://domination.fun", unsentMove: false } selection plan
        pure $ (Array.elem "This page will reload." lines == plan.reloadsThisPage
            && length lines >= 1)
          <?> show lines
    }
  , { name: "bulk confirmations warn about shared origins and unsent moves"
    , test: pure $ all_success
        [ assertEquals (hasLine "Other apps on" $ ask "https://example.github.io" false) true
        , assertEquals (hasLine "Other apps on" $ ask "https://domination.fun" false) false
        , assertEquals (hasLine "hasn't reached" $ ask "https://domination.fun" true) true
        ]
    }
  ]
  where
    roomContext = { room: GameRoom $ unsafeCode "abc", version: "v0.46.0", scope }
    roomItems = item roomContext <$>
      [ { locator: WebStorageKey Local "room/abc/game_state", size: Just 10 }
      , { locator: WebStorageKey Local "room/abc/chat", size: Just 10 }
      ]
    ask origin unsentMove = confirmation { origin, unsentMove } (WholeGroup ThisRoom)
      $ removalPlan roomContext roomItems (WholeGroup ThisRoom)
    hasLine text = any (contains $ Pattern text)
