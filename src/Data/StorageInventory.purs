-- | What the browser holds for this site, classified by who owns it. Discovery
-- | (Domination.Capability.StorageInspector) lists raw entries with no
-- | knowledge of Domination; this module recognizes them, groups them, sizes
-- | them, and plans removals, all without touching the browser.
module Domination.Data.StorageInventory
  ( Area(..)
  , AreaReport
  , Context
  , Discovery
  , Entry
  , Estimate
  , Freshness(..)
  , Group(..)
  , Item
  , Locator(..)
  , Measure(..)
  , Owner(..)
  , Plan
  , Removal(..)
  , Section
  , Selection(..)
  , Summary(..)
  , WebStorage(..)
  , allAreas
  , areaName
  , areaOf
  , classify
  , describeItem
  , discoveredEntries
  , emptyAreas
  , groupLabel
  , formatCount
  , formatSize
  , groupOf
  , inventory
  , isDeletable
  , item
  , locatorName
  , measureOf
  , removalPlan
  , roughShare
  , storageSummary
  , summarize
  , summaryOf
  , unavailableAreas
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array (filter, foldMap, length, null, sortBy)
import Data.Array as Array
import Data.Foldable (any, maximum)
import Data.Generic.Rep (class Generic)
import Data.Int (round)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), stripPrefix)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Data.Room (Room(..), RoomCode, roomCode, unRoomCode)
import Data.Either (Either(..), either)
import Domination.Data.StorageKey (DeviceKey(..), KeyDescription, StorageKey(..), describe, isHeldInMemory, parseKey)

data WebStorage = Local | Session

-- | Exactly what an area needs to find an entry again and delete it.
data Locator
  = WebStorageKey WebStorage String
  | Database { name :: String, version :: Int }
  | Cache String
  | Registration { scope :: String, script :: String }
  | OriginFile String
  | Cookie String

-- | `size` is in the area's measure; Nothing when the browser can't tell.
type Entry = { locator :: Locator, size :: Maybe Int }

data Area
  = LocalStorage
  | SessionStorage
  | IndexedDb
  | CacheStorage
  | ServiceWorkers
  | OriginFiles
  | Cookies

data Measure = Characters | Bytes

data Freshness = Current | Stale

data Owner
  = Known StorageKey
  | RoomData RoomCode String
  | AppShell Freshness
  | Unrecognized

data Group
  = ThisRoom
  | OtherRoom RoomCode
  | GlobalRoomGroup
  | Preferences
  | Diagnostics
  | AppFiles
  | NotRecognized
  | OtherArea Area

-- | What classification needs to know about the running page: its room, its
-- | version as the service worker names its cache (`v1.2.3`), and the scope
-- | its service worker registers.
type Context = { room :: Room, version :: String, scope :: String }

type Item = { locator :: Locator, size :: Maybe Int, owner :: Owner }

newtype Summary = Summary { entries :: Int, size :: Int, unknownSizes :: Int }

type Section = { group :: Group, summary :: Summary, items :: Array Item }

data Selection
  = OneItem Locator
  | WholeGroup Group
  | AllDomination
  | ReinstallAppFiles

-- | What one area held, or why the browser wouldn't say.
type AreaReport = { area :: Area, found :: Either String (Array Entry) }

-- | The browser's own estimate for the whole origin, all areas together.
type Estimate = { usage :: Number, quota :: Number, persisted :: Boolean }

type Discovery =
  { origin :: String
  , areas :: Array AreaReport
  , estimate :: Maybe Estimate
  }

-- | A database other pages hold open is deleted once they close it, so it
-- | is waiting rather than failed.
data Removal = Removed | Waiting String | Failed String

-- | `advancesGeneration`: the plan deletes data that pages hold in memory,
-- | so every other open page must stop writing and reload.
-- | `reloadsThisPage`: this page holds some of it too.
-- | `touchesRoomSync`: it deletes this room's game or mailbox bookkeeping.
type Plan =
  { items :: Array Item
  , advancesGeneration :: Boolean
  , reloadsThisPage :: Boolean
  , touchesRoomSync :: Boolean
  }

derive instance eqWebStorage :: Eq WebStorage
derive instance ordWebStorage :: Ord WebStorage
derive instance genericWebStorage :: Generic WebStorage _
instance showWebStorage :: Show WebStorage where
  show = genericShow

derive instance eqLocator :: Eq Locator
derive instance ordLocator :: Ord Locator
derive instance genericLocator :: Generic Locator _
instance showLocator :: Show Locator where
  show = genericShow

derive instance eqArea :: Eq Area
derive instance ordArea :: Ord Area
derive instance genericArea :: Generic Area _
instance showArea :: Show Area where
  show = genericShow

derive instance eqMeasure :: Eq Measure
derive instance genericMeasure :: Generic Measure _
instance showMeasure :: Show Measure where
  show = genericShow

derive instance eqFreshness :: Eq Freshness
derive instance genericFreshness :: Generic Freshness _
instance showFreshness :: Show Freshness where
  show = genericShow

derive instance eqOwner :: Eq Owner
derive instance genericOwner :: Generic Owner _
instance showOwner :: Show Owner where
  show = genericShow

derive instance eqGroup :: Eq Group
derive instance ordGroup :: Ord Group
derive instance genericGroup :: Generic Group _
instance showGroup :: Show Group where
  show = genericShow

derive instance eqSummary :: Eq Summary
derive instance genericSummary :: Generic Summary _
instance showSummary :: Show Summary where
  show = genericShow

instance semigroupSummary :: Semigroup Summary where
  append (Summary a) (Summary b) = Summary
    { entries: a.entries + b.entries
    , size: a.size + b.size
    , unknownSizes: a.unknownSizes + b.unknownSizes
    }

instance monoidSummary :: Monoid Summary where
  mempty = Summary { entries: 0, size: 0, unknownSizes: 0 }

allAreas :: Array Area
allAreas = [ LocalStorage, SessionStorage, IndexedDb, CacheStorage, ServiceWorkers, OriginFiles, Cookies ]

areaOf :: Locator -> Area
areaOf = case _ of
  WebStorageKey Local _ -> LocalStorage
  WebStorageKey Session _ -> SessionStorage
  Database _ -> IndexedDb
  Cache _ -> CacheStorage
  Registration _ -> ServiceWorkers
  OriginFile _ -> OriginFiles
  Cookie _ -> Cookies

areaName :: Area -> String
areaName = case _ of
  LocalStorage -> "local storage"
  SessionStorage -> "session storage"
  IndexedDb -> "IndexedDB"
  CacheStorage -> "Cache Storage"
  ServiceWorkers -> "service workers"
  OriginFiles -> "origin private files"
  Cookies -> "cookies"

measureOf :: Area -> Measure
measureOf = case _ of
  LocalStorage -> Characters
  SessionStorage -> Characters
  Cookies -> Characters
  _ -> Bytes

classify :: Context -> Locator -> Owner
classify context = case _ of
  WebStorageKey Local name -> maybe (roomData name) Known $ parseKey name
  Cache name | isVersionName name -> AppShell $ if name == context.version then Current else Stale
  Registration { scope } | scope == context.scope -> AppShell Current
  _ -> Unrecognized

-- | A key under a room's prefix whose name this version doesn't know.
roomData :: String -> Owner
roomData name = fromMaybe Unrecognized do
  rest <- stripPrefix (Pattern "room/") name
  at <- String.indexOf (Pattern "/") rest
  let before = String.take at rest
  code <- roomCode before
  guard $ unRoomCode code == before
  pure $ RoomData code $ String.drop (at + 1) rest

-- | `v` and three dot-separated numbers: how the service worker names caches.
isVersionName :: String -> Boolean
isVersionName name = maybe false isVersionNumbers $ stripPrefix (Pattern "v") name
  where
    isVersionNumbers = String.split (Pattern ".")
      >>> \parts -> length parts == 3 && Array.all isDigits parts
    isDigits part = part /= "" && Array.all (\c -> c >= '0' && c <= '9') (toCharArray part)

item :: Context -> Entry -> Item
item context { locator, size } = { locator, size, owner: classify context locator }

groupOf :: Context -> Item -> Group
groupOf context { locator, owner } = case owner of
  Known (InRoom room _) -> roomGroup context room
  RoomData code _ -> roomGroup context $ GameRoom code
  Known (OnDevice ConsoleLog) -> Diagnostics
  Known (OnDevice SyncJournal) -> Diagnostics
  Known (OnDevice _) -> Preferences
  AppShell _ -> AppFiles
  Unrecognized -> case areaOf locator of
    LocalStorage -> NotRecognized
    area -> OtherArea area

roomGroup :: Context -> Room -> Group
roomGroup context room
  | room == context.room = ThisRoom
  | otherwise = case room of
      GlobalRoom -> GlobalRoomGroup
      GameRoom code -> OtherRoom code

summarize :: forall r. { size :: Maybe Int | r } -> Summary
summarize { size } = Summary
  { entries: 1
  , size: fromMaybe 0 size
  , unknownSizes: maybe 1 (const 0) size
  }

summaryOf :: forall r. Array { size :: Maybe Int | r } -> Summary
summaryOf = foldMap summarize

sizeOf :: Summary -> Int
sizeOf (Summary { size }) = size

-- | Every entry in exactly one section; Domination's own groups first,
-- | largest first, then app files and other areas.
inventory :: Context -> Array Entry -> Array Section
inventory context entries = sortBy (comparing sectionRank)
  $ toSection <$> Map.toUnfoldable grouped
  where
    items = item context <$> entries
    grouped = Map.fromFoldableWith (flip append) $ (\i -> Tuple (groupOf context i) [ i ]) <$> items
    toSection (Tuple group groupItems) =
      { group
      , summary: summaryOf groupItems
      , items: sortBy (comparing (negate <<< fromMaybe (-1) <<< _.size)) groupItems
      }
    sectionRank { group, summary } = Tuple (groupTier group) (Tuple (negate $ sizeOf summary) group)

groupTier :: Group -> Int
groupTier = case _ of
  AppFiles -> 1
  OtherArea _ -> 2
  _ -> 0

-- | Whether the panel offers to delete this entry on its own: the clear
-- | counter must stay, the running app's files are only reinstalled, and
-- | cookies can't be deleted reliably from a script.
isDeletable :: Item -> Boolean
isDeletable { locator, owner } = case owner, locator of
  Known (OnDevice ClearGeneration), _ -> false
  AppShell Current, _ -> false
  _, Cookie _ -> false
  _, _ -> true

-- | Bulk clears only remove names Domination writes, or keys under one of
-- | its room prefixes.
isBulkRemovable :: Item -> Boolean
isBulkRemovable i = isDeletable i && case i.owner of
  Known _ -> true
  RoomData _ _ -> true
  _ -> false

isKnown :: Owner -> Boolean
isKnown = case _ of
  Known _ -> true
  _ -> false

isAppShell :: Owner -> Boolean
isAppShell = case _ of
  AppShell _ -> true
  _ -> false

removalPlan :: Context -> Array Item -> Selection -> Plan
removalPlan context items selection =
  { items: chosen
  , advancesGeneration: not $ null heldKeys
  , reloadsThisPage: selection == ReinstallAppFiles || any (heldHere context) heldKeys
  , touchesRoomSync: any (isRoomSync context) chosen
  }
  where
    chosen = filter (selects context selection) items
    heldKeys = Array.mapMaybe heldKey chosen

selects :: Context -> Selection -> Item -> Boolean
selects context = case _ of
  OneItem locator -> \i -> i.locator == locator && isDeletable i
  WholeGroup group -> \i -> groupOf context i == group && isBulkRemovable i
  AllDomination -> \i -> isKnown i.owner && isBulkRemovable i
  ReinstallAppFiles -> isAppShell <<< _.owner

heldKey :: Item -> Maybe StorageKey
heldKey { owner } = case owner of
  Known key | isHeldInMemory key -> Just key
  _ -> Nothing

heldHere :: Context -> StorageKey -> Boolean
heldHere context = case _ of
  InRoom room _ -> room == context.room
  OnDevice _ -> true

isRoomSync :: Context -> Item -> Boolean
isRoomSync context { owner } = case owner of
  Known key@(InRoom room _) -> room == context.room && isHeldInMemory key
  _ -> false

derive instance eqRemoval :: Eq Removal
derive instance genericRemoval :: Generic Removal _
instance showRemoval :: Show Removal where
  show = genericShow

discoveredEntries :: Discovery -> Array Entry
discoveredEntries = foldMap (either (const []) identity <<< _.found) <<< _.areas

emptyAreas :: Discovery -> Array Area
emptyAreas = map _.area <<< filter (either (const false) null <<< _.found) <<< _.areas

unavailableAreas :: Discovery -> Array (Tuple Area String)
unavailableAreas = Array.mapMaybe unavailable <<< _.areas
  where
    unavailable { area, found } = case found of
      Left reason -> Just $ Tuple area reason
      Right _ -> Nothing

-- | The name an entry has in its own area.
locatorName :: Locator -> String
locatorName = case _ of
  WebStorageKey _ name -> name
  Database { name, version } -> name <> " (version " <> show version <> ")"
  Cache name -> name
  Registration { scope } -> scope
  OriginFile name -> name
  Cookie name -> name

describeItem :: Item -> KeyDescription
describeItem { locator, owner } = case owner of
  Known key -> describe key
  RoomData _ name ->
    { label: name
    , consequence: "This version of Domination doesn't use it; a newer or older version may have saved it here."
    }
  AppShell Current ->
    { label: appShellLabel "current"
    , consequence: "Reinstalling downloads the app files again."
    }
  AppShell Stale ->
    { label: appShellLabel "old"
    , consequence: "Nothing: an older version of Domination left it behind."
    }
  Unrecognized ->
    { label: locatorName locator
    , consequence: "Domination doesn't know what wrote it, so whatever stored it may lose data."
    }
  where
    appShellLabel freshness = case locator of
      Registration _ -> "Service worker"
      _ -> "App files " <> locatorName locator <> " (" <> freshness <> ")"

groupLabel :: Group -> String
groupLabel = case _ of
  ThisRoom -> "This room"
  OtherRoom code -> "Room " <> unRoomCode code
  GlobalRoomGroup -> "Global room"
  Preferences -> "Preferences"
  Diagnostics -> "Diagnostics"
  AppFiles -> "App files"
  NotRecognized -> "Not recognized"
  OtherArea area -> String.toUpper (String.take 1 name) <> String.drop 1 name
    where
      name = areaName area

derive instance eqSelection :: Eq Selection
derive instance genericSelection :: Generic Selection _
instance showSelection :: Show Selection where
  show = genericShow

-- | `1284113` as `1,284,113`.
formatCount :: Int -> String
formatCount n
  | n < 0 = "-" <> formatCount (negate n)
  | otherwise = fromCharArray $ Array.reverse $ Array.concat
      $ Array.intersperse [ ',' ] $ chunksOf3 $ Array.reverse $ toCharArray $ show n
  where
    chunksOf3 chars
      | Array.length chars <= 3 = [ chars ]
      | otherwise = [ Array.take 3 chars ] <> chunksOf3 (Array.drop 3 chars)

-- | A size in its area's measure: characters exactly, bytes in round units.
formatSize :: Measure -> Number -> String
formatSize Characters n = formatCount (round n) <> " characters"
formatSize Bytes n = case Array.find (\(Tuple limit _) -> n < limit) byteUnits of
  Just (Tuple limit unit) | unit /= "bytes" -> oneDecimal (n * 1000.0 / limit) <> " " <> unit
  _ -> formatCount (round n) <> " bytes"
  where
    byteUnits =
      [ Tuple 1000.0 "bytes"
      , Tuple 1.0e6 "KB"
      , Tuple 1.0e9 "MB"
      , Tuple 1.0e12 "GB"
      , Tuple 1.0e15 "TB"
      ]
    oneDecimal x = let tenths = round (x * 10.0) in
      show (tenths / 10) <> "." <> show (tenths `mod` 10)

-- | A size as a rough share of a limit, for when the exact limit is the
-- | browser's own business.
roughShare :: Number -> Number -> String
roughShare limit size
  | size <= 0.0 = "none"
  | otherwise = describePercent $ round (size * 100.0 / limit)
  where
    describePercent percent
      | percent < 1 = "under 1%"
      | percent >= 100 = "all"
      | otherwise = "about " <> show percent <> "%"

-- | One bug-report line about localStorage, by category only: no key names
-- | and no room codes, since an old invite code still opens its room.
storageSummary :: Context -> Array Entry -> String
storageSummary context entries =
  formatCount total.size <> " chars in " <> show total.entries <> " keys; "
  <> show (Map.size rooms) <> " rooms"
  <> maybe "" (\largest -> " (largest " <> formatCount largest <> ")") (maximum $ Map.values rooms)
  <> category "global room" ThisRoom
  <> category "preferences" Preferences
  <> category "diagnostics" Diagnostics
  <> category "not recognized" NotRecognized
  where
    globalContext = context { room = GlobalRoom }
    items = filter ((_ == LocalStorage) <<< areaOf <<< _.locator) $ item globalContext <$> entries
    Summary total = summaryOf items
    grouped = (\i -> Tuple (groupOf globalContext i) i) <$> items
    rooms = Map.fromFoldableWith (+) $ Array.mapMaybe roomSize grouped
    roomSize (Tuple group i) = case group of
      OtherRoom code -> Just $ Tuple code $ fromMaybe 0 i.size
      _ -> Nothing
    category label group = "; " <> label <> " "
      <> formatCount (sizeOf $ summaryOf $ snd <$> filter ((_ == group) <<< fst) grouped)
