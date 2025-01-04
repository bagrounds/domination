module Domination.Capability.Storage where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Either as Either
import Domination.AppM (AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FFI (setItem)
import Halogen.Query.HalogenM (HalogenM)
import Util (mapLeft, readJson)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

class Monad m <= Storage m where
  save :: forall a. EncodeJson a => DecodeJson a => String -> a -> m (Either String Unit)
  load :: forall a. EncodeJson a => DecodeJson a => String -> m (Either String a)

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

saveStorage
  :: forall a m
  . MonadEffect m
  => EncodeJson a
  => DecodeJson a
  => String -> a -> m (Either String Unit)
saveStorage key value = liftEffect do
  result <- window >>= localStorage >>= setItem Left Right unit key stringValue
  pure $ contextualizeError `mapLeft` result
  where
    stringValue = (stringify <<< encodeJson) value
    contextualizeError error = "Error saving to local storage."
      <> " key: '" <> key <> "'"
      <> " value: '" <> stringValue <> "'"
      <> " error: '" <> error <> "'"

loadStorage
  :: forall a m
  . MonadEffect m
  => EncodeJson a
  => DecodeJson a
  => String -> m (Either String a)
loadStorage key = liftEffect $ do
  maybeItem <- window >>= localStorage >>= getItem key
  let eitherItemOrError = Either.note error maybeItem
  pure $ eitherItemOrError >>= readJson
  where
    error = "Error loading item from storage at key: '" <> key <> "'"
