module Domination.Capability.Storage where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Domination.AppM (AppM)
import Domination.Capability.Log (class Log, log)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Query.HalogenM (HalogenM)
import Util (readJson)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (clear, getItem, removeItem, setItem)

class Monad m <= Storage m where
  save :: forall a. EncodeJson a => DecodeJson a => String -> a -> m Unit
  load :: forall a. EncodeJson a => DecodeJson a => String -> m (Either String a)

instance storageHalogenM :: Storage m => Storage (HalogenM st act slots msg m) where
  save key = lift <<< save key
  load = lift <<< load

newtype StorageM a = StorageM (Aff a)

derive newtype instance functorStorageM :: Functor StorageM
derive newtype instance applyStorageM :: Apply StorageM
derive newtype instance applicativeStorageM :: Applicative StorageM
derive newtype instance bindStorageM :: Bind StorageM
derive newtype instance monadStorageM :: Monad StorageM
derive newtype instance monadEffectStorageM :: MonadEffect StorageM
derive newtype instance monadAffStorageM :: MonadAff StorageM

instance storageStorageM :: Storage StorageM where
  save key = liftEffect <<< saveStorage key
  load = liftEffect <<< loadStorage

runStorageM :: StorageM ~> Aff
runStorageM (StorageM m) = liftAff m

instance storageAppM :: Storage AppM where
  save key = liftEffect <<< saveStorage key
  load = liftEffect <<< loadStorage

saveStorage
  :: forall a m
  . MonadEffect m
  => EncodeJson a
  => DecodeJson a
  => String -> a -> m Unit
saveStorage key x = liftEffect do
  w <- window
  ls <- localStorage w
  let json = encodeJson x
  let string = stringify json
  setItem key string ls
loadStorage
  :: forall a m
  . MonadEffect m
  => EncodeJson a
  => DecodeJson a
  => String -> m (Either String a)
loadStorage key = liftEffect $ do
  w <- window
  ls <- localStorage w
  item <- getItem key ls
  pure $ case item of
    Nothing -> Left "Error retrieving item from storage"
    Just x -> readJson x

example :: forall m. MonadEffect m => Log m => m Unit
example = do
  w <- liftEffect window
  s <- liftEffect $ localStorage w
  liftEffect $ setItem "this-is-my-key" "Here is my value." s
  v <- liftEffect $ getItem "this-is-my-key" s
  log $ show v

  liftEffect $ removeItem "this-is-my-key" s
  v' <- liftEffect $ getItem "this-is-my-key" s
  log "It is gone!"
  log $ show v'

  liftEffect $ clear s
