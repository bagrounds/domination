--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A PureScript module providing a basic storage capability for storing and retrieving data locally via JavaScript's localStorage.
--|
--| ### Key Concepts
--| * **Storage Monad**: A monad for storing data in local storage.
--| * **Effectful Storage**: The `StorageM` type is an effectful version of the traditional storage monad, allowing for side effects like interacting with local storage.
--| * **Local Storage API**: The module provides a set of functions for saving and loading data to/from local storage using the Web Storage API.
module Domination.Capability.Storage where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String as String
import Domination.AppM (AppM)
import Domination.Capability.ClearGeneration as ClearGeneration
import Domination.Data.StorageError (BrowserFailure, LoadError(..), SaveError(..), describeBrowserFailure, printSaveError, saveError)
import Domination.Data.StorageKey (StorageKey, printKey)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Query.HalogenM (HalogenM)
import Util (mapLeft, readJson)

class Monad m <= Storage m where
  save :: forall a. EncodeJson a => DecodeJson a => StorageKey -> a -> m (Either SaveError Unit)
  load :: forall a. EncodeJson a => DecodeJson a => StorageKey -> m (Either LoadError a)

instance storageHalogenM :: Storage m => Storage (HalogenM st act slots msg m) where
  save key = lift <<< save key
  load = lift <<< load

newtype StorageM a = StorageM (Effect a)

derive newtype instance functorStorageM :: Functor StorageM
derive newtype instance applyStorageM :: Apply StorageM
derive newtype instance applicativeStorageM :: Applicative StorageM
derive newtype instance bindStorageM :: Bind StorageM
derive newtype instance monadStorageM :: Monad StorageM
derive newtype instance monadEffectStorageM :: MonadEffect StorageM

instance storageStorageM :: Storage StorageM where
  save key = liftEffect <<< saveStorage key
  load = liftEffect <<< loadStorage

runStorageM :: StorageM ~> Effect
runStorageM (StorageM m) = liftEffect m

instance storageAppM :: Storage AppM where
  save key = liftEffect <<< saveStorage key
  load = liftEffect <<< loadStorage

foreign import setItemFFI
  :: (forall r. BrowserFailure -> Either BrowserFailure r)
  -> (forall l. Either l Unit)
  -> String
  -> String
  -> Effect (Either BrowserFailure Unit)

foreign import getItemFFI
  :: (forall r. BrowserFailure -> Either BrowserFailure r)
  -> (forall l r. r -> Either l r)
  -> (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> String
  -> Effect (Either BrowserFailure (Maybe String))

-- | Writes nothing once the Storage panel has deleted data this page holds
-- | (Domination.Capability.ClearGeneration): the page must reload instead.
saveStorage
  :: forall a m
  . MonadEffect m
  => EncodeJson a
  => DecodeJson a
  => StorageKey -> a -> m (Either SaveError Unit)
saveStorage storageKey value = liftEffect do
  current <- ClearGeneration.isCurrent
  if current then write else pure $ Left $ Superseded { key }
  where
    key = printKey storageKey
    stringValue = (stringify <<< encodeJson) value
    write = mapLeft (saveError { key, chars: String.length stringValue }) <$> setItemFFI Left (Right unit) key stringValue

-- | The log line for a failed save of `value` at `key`, failing with
-- | `browserError`.
saveFailureMessage :: String -> String -> String -> String
saveFailureMessage key value browserError = printSaveError
  $ SaveRejected { key, chars: String.length value, detail: browserError }

loadStorage
  :: forall a m
  . MonadEffect m
  => EncodeJson a
  => DecodeJson a
  => StorageKey -> m (Either LoadError a)
loadStorage storageKey = liftEffect do
  result <- getItemFFI Left Right Just Nothing key
  pure $ unavailable `mapLeft` result >>= note (Absent { key }) >>= (mapLeft unreadable <<< readJson)
  where
    key = printKey storageKey
    unavailable failure = LoadUnavailable { key, detail: describeBrowserFailure failure }
    unreadable detail = Unreadable { key, detail }
