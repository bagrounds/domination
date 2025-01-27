module Domination.Capability.Broadcast.WebSocket where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either)
import Effect.Aff (Aff, Error, makeAff)
import Effect.Class (liftEffect)
import FFI.WebSocket as FFI

newtype WebSocketBroadcaster = WebSocketBroadcaster FFI.WebSocket

instance showWebSocketBroadcaster :: Show WebSocketBroadcaster where
  show (WebSocketBroadcaster bugout) = FFI.showWebSocket bugout

broadcastWebSocketMessage :: WebSocketBroadcaster -> String -> Aff Unit
broadcastWebSocketMessage (WebSocketBroadcaster bugout) = liftEffect <<< FFI.send bugout

createWebSocketBroadcaster :: String -> String -> String -> String -> Aff (Either Error WebSocketBroadcaster)
createWebSocketBroadcaster roomCode remoteMessageTarget localMessageTarget announce =
  try $ WebSocketBroadcaster <$> makeAff
    (FFI.makeWebSocket roomCode remoteMessageTarget localMessageTarget announce)

getWebSocketAddress :: WebSocketBroadcaster -> Aff String
getWebSocketAddress (WebSocketBroadcaster bugout) = liftEffect $ FFI.address bugout
