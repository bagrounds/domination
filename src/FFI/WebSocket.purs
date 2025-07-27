module FFI.WebSocket where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Canceler, Error)
import Web.Event.Event (Event)

foreign import detail :: forall a. Event -> a

foreign import data WebSocket :: Type

foreign import showWebSocket :: WebSocket -> String

makeWebSocket
  :: String -- remote message target
  -> String -- room code
  -> String -- serverUrl
  -> (Either Error WebSocket -> Effect Unit)
  -> Effect Canceler
makeWebSocket = makeWebSocketFFI
  Left
  Right

foreign import makeWebSocketFFI
  :: (forall l r. l -> Either l r) -- Left
  -> (forall l r. r -> Either l r) -- Right
  -> String
  -> String
  -> String
  -> (Either Error WebSocket -> Effect Unit)
  -> Effect Canceler

foreign import send :: WebSocket -> String -> Effect Unit

foreign import address :: WebSocket -> Effect String

foreign import cleanup :: WebSocket -> Effect Unit

foreign import onClose :: WebSocket -> Effect Unit -> Effect Unit
