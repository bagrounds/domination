module FFI where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Canceler, Error)
import Message (LocalMessage(..))
import Web.Event.Event (Event)

foreign import registerServiceWorker :: Effect Unit

foreign import copyToClipboard :: String -> Effect Unit

foreign import detail :: forall a. Event -> a

foreign import data Bugout :: Type

makeBugout
  :: String -- remote message target
  -> String -- local message target
  -> String -- room code
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

