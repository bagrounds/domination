module Storage
  ( save
  , load
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Domination.Capability.Log (class Log, log)
import Effect.Class (class MonadEffect, liftEffect)
import Util (readJson)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (clear, getItem, removeItem, setItem)

save :: forall m a.
  MonadEffect m =>
  EncodeJson a =>
  DecodeJson a =>
  String -> a -> m Unit
save key x = liftEffect do
  w <- window
  ls <- localStorage w
  let json = encodeJson x
  let string = stringify json
  setItem key string ls

load :: forall m a.
  MonadEffect m =>
  EncodeJson a =>
  DecodeJson a =>
  String -> m (Either String a)
load key = liftEffect $ do
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
