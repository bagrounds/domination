module FFI.WebSocket where

import Prelude

import Data.Either (Either(..))
import Domination.Data.Backoff (delay, reconnectBackoff)
import Effect (Effect)
import Effect.Aff (Error, Milliseconds)
import Web.Event.Event (Event)

foreign import detail :: forall a. Event -> a

-- | A connection to the relay that reconnects by itself. It dispatches
-- | `purescript` events for received messages, and `disconnected` and
-- | `reconnected` events, on the remote message target.
foreign import data Connection :: Type

foreign import showConnection :: Connection -> String

makeConnection
  :: String -- remote message target
  -> String -- serverUrl
  -> (Either Error Connection -> Effect Unit)
  -> Effect Unit
makeConnection = makeConnectionFFI Left Right (delay reconnectBackoff)

foreign import makeConnectionFFI
  :: (forall l r. l -> Either l r) -- Left
  -> (forall l r. r -> Either l r) -- Right
  -> (Int -> Number -> Milliseconds) -- reconnect delay for an attempt and a random sample
  -> String
  -> String
  -> (Either Error Connection -> Effect Unit)
  -> Effect Unit

foreign import send :: Connection -> String -> Effect Unit

foreign import address :: Connection -> Effect String
