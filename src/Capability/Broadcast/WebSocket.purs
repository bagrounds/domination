module Domination.Capability.Broadcast.WebSocket where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Effect.Aff (Aff, Error, makeAff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Time.Duration (Milliseconds(..))
import FFI.WebSocket as FFI

newtype WebSocketBroadcaster = WebSocketBroadcaster
  { socket :: Ref.Ref FFI.WebSocket
  , roomCode :: String
  , remoteMessageTarget :: String
  , serverUrl :: String
  }

-- Fix the Show instance to return a pure String
instance showWebSocketBroadcaster :: Show WebSocketBroadcaster where
  show (WebSocketBroadcaster { serverUrl }) = 
    "WebSocketBroadcaster(" <> serverUrl <> ")"

-- Alternative function to get a string representation with effects
getWebSocketDisplayString :: WebSocketBroadcaster -> Aff String
getWebSocketDisplayString (WebSocketBroadcaster { socket }) = do
  ws <- liftEffect $ Ref.read socket
  liftEffect $ pure $ FFI.showWebSocket ws

-- Fix the createWebSocket function to properly use makeAff
createWebSocket :: String -> String -> String -> Aff FFI.WebSocket
createWebSocket roomCode remoteMessageTarget serverUrl = 
  makeAff \callback -> 
    FFI.makeWebSocketFFI Left Right roomCode remoteMessageTarget serverUrl callback

broadcastWebSocketMessage :: WebSocketBroadcaster -> String -> Aff Unit
broadcastWebSocketMessage (WebSocketBroadcaster { socket }) msg = do
  ws <- liftEffect $ Ref.read socket
  liftEffect $ FFI.send ws msg

createWebSocketBroadcaster :: String -> String -> String -> Aff (Either Error WebSocketBroadcaster)
createWebSocketBroadcaster roomCode remoteMessageTarget serverUrl = try do
  ws <- createWebSocket roomCode remoteMessageTarget serverUrl
  socketRef <- liftEffect $ Ref.new ws
  let broadcaster = WebSocketBroadcaster
        { socket: socketRef
        , roomCode
        , remoteMessageTarget
        , serverUrl
        }
  setupReconnection broadcaster
  pure broadcaster

getWebSocketAddress :: WebSocketBroadcaster -> Aff String
getWebSocketAddress (WebSocketBroadcaster { socket }) = do
  ws <- liftEffect $ Ref.read socket
  liftEffect $ FFI.address ws

setupReconnection :: WebSocketBroadcaster -> Aff Unit
setupReconnection wb@(WebSocketBroadcaster { socket, roomCode, remoteMessageTarget, serverUrl }) = do
  ws <- liftEffect $ Ref.read socket
  liftEffect $ FFI.onClose ws $ void $ launchAff_ do
    delay (Milliseconds 1000.0) -- Wait 1 second before reconnecting
    newWs <- createWebSocket roomCode remoteMessageTarget serverUrl
    liftEffect $ Ref.write newWs socket
    setupReconnection wb
