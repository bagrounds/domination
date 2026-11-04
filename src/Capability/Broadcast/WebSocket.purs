module Domination.Capability.Broadcast.WebSocket where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either)
import Effect.Aff (Aff, Error, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import FFI.WebSocket as FFI

newtype WebSocketBroadcaster = WebSocketBroadcaster FFI.Connection

instance showWebSocketBroadcaster :: Show WebSocketBroadcaster where
  show (WebSocketBroadcaster connection) = FFI.showConnection connection

broadcastWebSocketMessage :: WebSocketBroadcaster -> String -> Aff Unit
broadcastWebSocketMessage (WebSocketBroadcaster connection) = liftEffect <<< FFI.send connection

createWebSocketBroadcaster :: String -> String -> Aff (Either Error WebSocketBroadcaster)
createWebSocketBroadcaster remoteMessageTarget serverUrl =
  try $ WebSocketBroadcaster <$> makeAff \callback ->
    FFI.makeConnection remoteMessageTarget serverUrl callback $> nonCanceler

getWebSocketAddress :: WebSocketBroadcaster -> Aff String
getWebSocketAddress (WebSocketBroadcaster connection) = liftEffect $ FFI.address connection

reopenWebSocket :: WebSocketBroadcaster -> Aff Unit
reopenWebSocket (WebSocketBroadcaster connection) = liftEffect $ FFI.reopen connection
