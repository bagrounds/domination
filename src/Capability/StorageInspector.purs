-- | The browser edge of the Storage panel: lists what this origin stores in
-- | every area a page can see, and removes entries by their locators.
-- | Domination.Data.StorageInventory makes sense of the results.
module Domination.Capability.StorageInspector
  ( discover
  , isOnline
  , localStorageEntries
  , origin
  , readValue
  , remove
  , serviceWorkerScope
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Domination.Data.StorageInventory (Area(..), Discovery, Entry, Estimate, Locator(..), Removal(..), WebStorage(..), allAreas)

type Named = { name :: String, size :: Int }

type MaybeSized = { name :: String, size :: Maybe Int }

type Failable a result = (String -> Either String a) -> (a -> Either String a) -> result

type Listing a = Failable (Array a) (Effect (Either String (Array a)))

type AsyncListing a = Failable (Array a) (EffectFnAff (Either String (Array a)))

foreign import localEntries :: Listing Named
foreign import sessionEntries :: Listing Named
foreign import cookieEntries :: Listing Named
foreign import databaseEntriesImpl :: AsyncListing { name :: String, version :: Int }
foreign import cacheEntriesImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> AsyncListing MaybeSized
foreign import registrationEntriesImpl :: AsyncListing { scope :: String, script :: String }
foreign import fileEntriesImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> AsyncListing MaybeSized

foreign import estimateImpl
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> EffectFnAff (Maybe Estimate)

foreign import readWebStorageImpl
  :: String
  -> (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> String
  -> Effect (Maybe String)

type Remover = Removal -> (String -> Removal) -> (String -> Removal) -> EffectFnAff Removal

foreign import removeWebStorageImpl :: String -> String -> Remover
foreign import deleteDatabaseImpl :: String -> Remover
foreign import deleteCacheImpl :: String -> Remover
foreign import unregisterImpl :: String -> Remover
foreign import removeFileImpl :: String -> Remover

foreign import origin :: Effect String

foreign import isOnline :: Effect Boolean

-- | The scope Domination's service worker registers with on this page.
foreign import serviceWorkerScope :: Effect String

failable :: forall a result. Failable a result -> result
failable f = f Left Right

webStorageName :: WebStorage -> String
webStorageName = case _ of
  Local -> "local"
  Session -> "session"

sized :: (String -> Locator) -> Named -> Entry
sized locator { name, size } = { locator: locator name, size: Just size }

-- | localStorage alone, synchronously, for the bug report's summary line.
localStorageEntries :: Effect (Array Entry)
localStorageEntries = either (const []) (map $ sized $ WebStorageKey Local) <$> failable localEntries

discoverArea :: Area -> Aff (Either String (Array Entry))
discoverArea = case _ of
  LocalStorage -> liftEffect $ map (map $ sized $ WebStorageKey Local) <$> failable localEntries
  SessionStorage -> liftEffect $ map (map $ sized $ WebStorageKey Session) <$> failable sessionEntries
  Cookies -> liftEffect $ map (map $ sized Cookie) <$> failable cookieEntries
  IndexedDb -> map (map \database -> { locator: Database database, size: Nothing })
    <$> fromEffectFnAff (failable databaseEntriesImpl)
  CacheStorage -> map (map \{ name, size } -> { locator: Cache name, size })
    <$> fromEffectFnAff (failable $ cacheEntriesImpl Just Nothing)
  ServiceWorkers -> map (map \registration -> { locator: Registration registration, size: Nothing })
    <$> fromEffectFnAff (failable registrationEntriesImpl)
  OriginFiles -> map (map \{ name, size } -> { locator: OriginFile name, size })
    <$> fromEffectFnAff (failable $ fileEntriesImpl Just Nothing)

discover :: Aff Discovery
discover = { origin: _, areas: _, estimate: _ }
  <$> liftEffect origin
  <*> traverse (\area -> { area, found: _ } <$> discoverArea area) allAreas
  <*> fromEffectFnAff (estimateImpl Just Nothing)

-- | A Web Storage value, for showing or copying; other areas have none.
readValue :: Locator -> Effect (Maybe String)
readValue = case _ of
  WebStorageKey which name -> readWebStorageImpl (webStorageName which) Just Nothing name
  _ -> pure Nothing

remover :: Locator -> Maybe Remover
remover = case _ of
  WebStorageKey which name -> Just $ removeWebStorageImpl (webStorageName which) name
  Database { name } -> Just $ deleteDatabaseImpl name
  Cache name -> Just $ deleteCacheImpl name
  Registration { scope } -> Just $ unregisterImpl scope
  OriginFile name -> Just $ removeFileImpl name
  Cookie _ -> Nothing

removeOne :: Locator -> Aff Removal
removeOne = remover >>> maybe
  (pure $ Failed "a script can't reliably remove cookies")
  \removeIt -> fromEffectFnAff $ removeIt Removed Waiting Failed

-- | Removes each entry in turn, reporting how each went.
remove :: Array Locator -> Aff (Array (Tuple Locator Removal))
remove = traverse \locator -> Tuple locator <$> removeOne locator
