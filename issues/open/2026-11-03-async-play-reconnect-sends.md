# Async Play: Reconnect Sends on the Live Socket

**Source:** [Async play design](../docs/2026-11-03-async-play-design.md) §3.3 and §6.3

**Status:** Open

## Description

After the relay restarts (Render deploy, spin-down or network blip), every client
reconnects, but it keeps **sending on the old, closed socket**. Incoming messages still
arrive. Outgoing chat messages, moves and heartbeats are silently dropped until the page is
reloaded. Chrome logs `WebSocket is already in CLOSING or CLOSED state.`

I reproduced this with two browsers and a restarted local relay (design §3.3, E2).

Cause:
- `Initialize` keeps the socket from the first `onopen` in `AppState.maybeBroadcaster`.
- The reconnect in `src/FFI/WebSocket.js` calls the same `makeAff` callback again.
- `Aff` ignores a second resume, so the app state keeps the dead socket.

Also, the reconnect loop calls `alert()` every 5 s while the server is unreachable.

## Approach

- `send` uses the current singleton socket (`getWebSocket()`), not a captured one.
- Queue messages while the socket is `CONNECTING` or closed, and flush the queue on
  `onopen`. `send` throws `InvalidStateError` while `CONNECTING`.
- After a reconnect, dispatch a `reconnected` event on `#remote-message-target`. `Main`
  handles it by re-sending `JoinMessage` and `UsernameMessage`, then pulling the mailbox
  or doing catch-up on join.
- Retry with exponential backoff and jitter instead of a fixed 5 s.
- Replace the `alert()` calls with a log line and an offline indicator.

## Acceptance Criteria

- [ ] After a relay restart, chat messages and moves flow in both directions without a
      reload.
- [ ] Peers see each other again in the connection count after a reconnect.
- [ ] No alert dialogs while the server is unreachable.
- [ ] Backoff delays are covered by a pure, property-tested function.
