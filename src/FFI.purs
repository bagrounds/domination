{-|
Module for Foreign Function Interface (FFI) bindings.

This module provides FFI bindings for various JavaScript functions
used in the application, such as service worker registration and
clipboard operations.

Key Components:
- registerServiceWorker: Registers a service worker
- copyToClipboard: Copies a string to the clipboard
- detail: Extracts detail from a JavaScript event
- Bugout: Foreign data type for Bugout library
- makeBugout: Creates a Bugout instance for peer-to-peer communication

Usage:
Import this module to use the provided FFI bindings in your application.
-}

module FFI where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Canceler, Error)
import Message (LocalMessage(..))
import Web.Event.Event (Event)
import Web.Storage.Storage (Storage)

foreign import registerServiceWorker :: Effect Unit

foreign import copyToClipboard :: String -> Effect Unit

foreign import detail :: forall a. Event -> a

foreign import data Bugout :: Type

foreign import showBugout :: Bugout -> String

makeBugout
  :: String -- remote message target
  -> String -- local message target
  -> String -- room code
  -> String -- announce
  -> (Either Error Bugout -> Effect Unit)
  -> Effect Canceler
makeBugout = makeBugoutFFI
  Left
  Right
  ConnectionsMessage
  SeenMessage

foreign import makeBugoutFFI
  :: (forall l r. l -> Either l r) -- Left
  -> (forall l r. r -> Either l r) -- Right
  -> (Int -> LocalMessage) -- ConnectionsWireMessage
  -> (String -> LocalMessage) -- SeenWireMessage
  -> String
  -> String
  -> String
  -> String
  -> (Either Error Bugout -> Effect Unit)
  -> Effect Canceler

foreign import send :: Bugout -> String -> Effect Unit

foreign import address :: Bugout -> Effect String

foreign import genUuid :: Effect String

foreign import arrayBufferAsString :: ArrayBuffer -> String

foreign import stringAsArrayBuffer :: String -> ArrayBuffer

foreign import compressString :: String -> String

decompressString :: String -> Maybe String
decompressString = decompressStringFFI Just Nothing

foreign import decompressStringFFI
  :: (forall x. x -> Maybe x)
  -> (forall x. Maybe x)
  -> String
  -> Maybe String

foreign import setItem
  :: (forall l r. l -> Either l r) -- left constructor
  -> (forall l r. r -> Either l r) -- right constructor
  -> Unit -- in case we need to return unit
  -> String -- key
  -> String -- value
  -> Storage -- local storage
  -> Effect (Either String Unit) -- error message or nothing (unit)
