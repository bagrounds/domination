# Async Play: Room Mailbox

**Source:** [Async play design](../docs/2026-11-03-async-play-design.md) §6.4

**Setup:** [Cloudflare account, token and GitHub secrets](../docs/2026-11-03-async-play-cloudflare-setup.md)

**Status:** Open

## Description

Store the latest game envelope of each game room somewhere that stays online, so a player
who comes back gets the moves they missed. This works like an MQTT retained message or
Game Center's `matchData`.

## Approach

**Server** (`mailbox/`, a Cloudflare Worker with one SQLite-backed Durable Object per room,
on the free plan):
- `GET /v1/rooms/{code}` with optional `If-None-Match: "<revision>"`. Returns 200, 204
  (no game yet) or 304.
- `PUT /v1/rooms/{code}` with `If-Match: "<revision>"`. Returns 200 with the next
  revision, or 412 with the current envelope and revision. A `PUT` without `If-Match`
  returns 428.
- Revisions start at 0 and never repeat. Expiry after 90 days without a write deletes the
  envelope and advances the revision.
- Bodies are limited to 16 KiB. Room codes must match `[a-z0-9]{1,32}`.
- CORS for the app origins, exposing `ETag`. Responses set `Cache-Control: no-store`.
- Every decision is made by a pure `decide` function. The Durable Object is only the shell.

**Client:**
- A header stamp in the sender segment: `@r` means committed as revision r, `~r` means
  not committed and based on r. Old builds read it as part of the sender, which they
  ignore.
- Pure `Domination.Data.Mailbox`: `Revision`, `Stamp`, `Dirty = Clean | Moved | Relaying`,
  `Sync { confirmed, dirty }`, `receiveFrame`, `resolve`.
- Invariant: a `Clean` client's game is the committed state at `confirmed`, and any other
  game is being delivered. `confirmed` never decreases, and replies or frames at or below
  it are ignored.
- Live frames: adopt `@r` only if `r > confirmed` (becoming `Clean`), and `~b` only if
  `b ≥ confirmed`. Unstamped frames from old builds are adopted as today. An adopted `~`
  frame (or an unstamped one before the first commit) makes the receiver `Relaying`: it
  delivers the frame as a courier, so a move whose author left still reaches the mailbox.
- Retry deliveries with backoff while not `Clean`. Expiry advances the revision, and a
  client that still has the game saved delivers it again.
- Pull on `Initialize`, `RelayRegained`, `online` and `visibilitychange`, with
  `If-None-Match: confirmed`. Replay through the existing receive path.
- Deliver every own game change with `If-Match: confirmed`, then broadcast it.
  - If the mailbox is unreachable, broadcast it stamped `~` and keep `dirty`.
  - On a 200 that arrives after a frame was adopted, replay the accepted envelope: the
    committed state wins over an uncommitted one.
  - On 412, if the envelope is one of our own uncertain writes, move `confirmed` forward
    and retry. Otherwise adopt the envelope and tell the player their move was not
    applied.
- *Mailbox URL* in Settings → Connection. An empty value turns async play off.
- A small synced, pending or offline mark next to the connection count.

## Acceptance Criteria

- [ ] B is offline. A plays and closes the tab, and the relay restarts. B opens the room
      and sees A's move.
- [ ] A's write fails, A broadcasts and leaves, and C is offline. C later sees A's move,
      delivered by B.
- [ ] A plays while offline and reloads. The move is delivered when A is back online.
- [ ] A write based on an old revision is refused, and the writer adopts the newer state.
- [ ] A late live frame with an old revision is ignored.
- [ ] Live play with old builds is unchanged. The global room has no mailbox.
- [ ] Property tests for stamps, `receiveFrame`, a multi-client model test, and the
      server's `decide` (compare-and-set model test).
