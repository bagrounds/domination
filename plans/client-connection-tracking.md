# Network Protocol Design for Deck Builder Card Game Web App

## 1. Functional Requirements

- The server should remain stateless and only relay messages between clients.
- Clients must be able to track the number of active connections without relying on the server.
- Each client should announce its presence upon connection.
- Clients should periodically send heartbeat messages to indicate they are still active.
- Clients should remove inactive peers after a defined timeout period.
- Clients should send a leave message upon disconnection when possible.
- All game-related and chat messages must continue to function as before.

## 2. Non-Functional Requirements

- The protocol should be transport-agnostic, supporting WebSockets and potentially other message-passing systems.
- The protocol should allow for minimal client-server coupling to enable future backend flexibility.
- The system should tolerate network delays and ensure eventual consistency in peer connection state tracking.
- The protocol should be extendable for future game-related features without breaking existing functionality.

## 3. Data Schema

```purescript
data RemoteMessage
  = ChatMessage { username :: String, message :: String, chatNumber :: Int }
  | UsernameMessage { username :: String, id :: String }
  | GameMessage
    { i :: Int
    , state :: Game
    , playMade :: Maybe
      { play :: Play
      , playerIndex :: Int
      , state :: Game
      }
    }
  | PlayMadeMessage
    { play :: Play
    , playerIndex :: Int
    , state :: Game
    }
  | JoinMessage { clientId :: String }
  | HeartbeatMessage { clientId :: String, timestamp :: Int }
  | LeaveMessage { clientId :: String }
```

## 4. API

### Client-Side API

```purescript
sendJoinMessage :: WebSocket -> String -> Effect Unit
sendJoinMessage ws clientId =
  send ws (JoinMessage { clientId })

sendHeartbeatMessage :: WebSocket -> String -> Effect Unit
sendHeartbeatMessage ws clientId = do
  timestamp <- getCurrentTimestamp
  send ws (HeartbeatMessage { clientId, timestamp })

sendLeaveMessage :: WebSocket -> String -> Effect Unit
sendLeaveMessage ws clientId =
  send ws (LeaveMessage { clientId })
```

### Handling Incoming Messages

```purescript
handleMessage :: RemoteMessage -> State -> Effect State
handleMessage (JoinMessage { clientId }) state =
  addClient clientId state

handleMessage (HeartbeatMessage { clientId, timestamp }) state =
  updateClientHeartbeat clientId timestamp state

handleMessage (LeaveMessage { clientId }) state =
  removeClient clientId state

handleMessage otherMessage state =
  processGameOrChatMessage otherMessage state
```

## 5. Protocol

1. **Client Connection**
   - Upon connecting to the WebSocket server, a client sends a `JoinMessage`.
   - All clients receiving the `JoinMessage` add the sender to their active peer list.

2. **Heartbeat Mechanism**
   - Every client periodically sends a `HeartbeatMessage`.
   - Clients update their peer list upon receiving a `HeartbeatMessage`.
   - If a client does not receive a heartbeat from a peer within a timeout window (e.g., 15 seconds), it removes that peer from its list.

3. **Graceful Disconnect**
   - When a client disconnects normally, it sends a `LeaveMessage`.
   - All clients remove the sender from their peer list upon receiving the `LeaveMessage`.

4. **Failure Handling**
   - If a client disconnects unexpectedly (e.g., network failure), peers will eventually remove it based on the absence of heartbeat messages.

This protocol ensures that clients independently track active connections, preserving the stateless nature of the server while maintaining a synchronized client state.

# Client Connection Tracking Implementation Plan

