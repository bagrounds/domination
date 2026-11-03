# Async Play: Room Mailbox

**Source:** [Async play design](../docs/2026-11-03-async-play-design.md) §6.4

**Status:** Open

## Description

Store the latest game envelope of each game room somewhere that stays online, so a player
who comes back gets the moves they missed. This works like an MQTT retained message or
Game Center's `matchData`.

## Approach

**Server** (`mailbox/`, a Cloudflare Worker with one SQLite-backed Durable Object per room,
on the free plan):
- `GET /v1/rooms/{code}` with optional `If-None-Match`. Returns 200, 304 or 404.
- `PUT /v1/rooms/{code}` with `If-Match: <tag>` or `If-None-Match: *`. Returns 200 or 201,
  or 412 with the current envelope. A `PUT` with neither header returns 428.
- The tag is `"` + base64url(SHA-256(body)) + `"`.
- Bodies are limited to 16 KiB. Room codes must match `[a-z0-9]{1,32}`.
- CORS for the app origins, exposing `ETag`. Responses set `Cache-Control: no-store`.
- Rooms are deleted 90 days after the last write, using a Durable Object alarm.
- Every decision is made by a pure `decide` function. The Durable Object is only the shell.

**Client:**
- Pure `Domination.Data.Mailbox`: tags, bounded seen tags, a single-slot outbox,
  preconditions and conflict `resolve`.
- `Mailbox` capability with `fetch` and `crypto.subtle` through FFI.
- Pull on `Initialize`, `Reconnected`, `online` and `visibilitychange`. Replay through
  the existing receive path.
- Write-through: on every own game change in a game room, `PUT` and then broadcast. If
  the mailbox is unreachable, broadcast anyway and keep the write in the persisted outbox.
- On 412:
  - If the client has already seen the mailbox's state, fast-forward: retry with that tag.
  - Otherwise adopt the mailbox's state, re-broadcast it and tell the player their move
    was not applied.
- *Mailbox URL* in Settings → Connection. An empty value turns async play off.
- A small synced, pending or offline mark next to the connection count.

## Acceptance Criteria

- [ ] B is offline. A plays and closes the tab, and the relay restarts. B opens the room
      and sees A's move.
- [ ] A plays while offline and reloads. The move is delivered when A is back online.
- [ ] A write based on a state the mailbox has moved past is refused, and the writer adopts
      the mailbox's state.
- [ ] Live play with old builds is unchanged. The global room has no mailbox.
- [ ] Property tests for `resolve`, the seen tags, the outbox and the server's `decide`
      (compare-and-set model test).
