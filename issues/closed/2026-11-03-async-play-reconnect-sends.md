# Async Play: Reconnect Sends on the Live Socket

**Source:** [Async play design](../docs/2026-11-03-async-play-design.md) §3.3 and §6.3

**Status:** Closed (fixed on 2026-11-03)

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

- [x] After a relay restart, chat messages and moves flow in both directions without a
      reload.
- [x] Peers see each other again in the connection count after a reconnect.
- [x] No alert dialogs while the server is unreachable.
- [x] Backoff delays are covered by a pure, property-tested function.

## Resolution

- `src/FFI/WebSocket.js` keeps one connection per page that outlives its sockets. `send`
  uses the live socket and queues up to 100 messages while none is open. The queue is
  flushed on `onopen`.
- Every unexpected close reconnects, including `1001` from a server that shuts down. The
  delay comes from the pure `Domination.Data.Backoff` (1 s doubling to 60 s, equal jitter),
  covered by property tests in `test/Backoff.purs`. The `online` event skips the wait.
- The connection dispatches `disconnected` and `reconnected` events. `Main` handles them
  with `RelayLost` (offline mark on the connection count, heartbeats paused) and
  `RelayRegained` (re-sends `JoinMessage` and `UsernameMessage`).
- Errors are logged instead of alerted.
- Verified by hand with two tabs and a restarted local relay (design §6.3). A message that
  is flushed before the other peer has reconnected is still lost, because the relay keeps
  nothing. The [room mailbox](../open/2026-11-03-async-play-mailbox.md) fixes that.
