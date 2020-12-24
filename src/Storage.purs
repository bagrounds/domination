module Storage
  ( save
  , load
  ) where

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, logShow)
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

readJson :: forall a. DecodeJson a => String -> Either String a
readJson = lmap show <<< decodeJson <=< jsonParser

example :: forall m. MonadEffect m => m Unit
example = liftEffect do
  w <- window
  s <- localStorage w
  setItem "this-is-my-key" "Here is my value." s
  v <- getItem "this-is-my-key" s
  logShow v

  removeItem "this-is-my-key" s
  v' <- getItem "this-is-my-key" s
  log "It is gone!"
  logShow v'

  clear s
