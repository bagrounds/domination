# Matchmaking: Discover and Join Unstarted Games

**Source:** [Matchmaking design](../docs/2026-10-31-matchmaking-design.md) §6.1 (iteration 2)

**Status:** Open

**Depends on:** invite-link rooms (MVP, done)

## Description

Let players in the global room see open, unstarted private games and join them, without
sharing a link out of band. Follow the Steam/EOS lobby model: list, filter, join, wait,
start.

## Scope

- **Opt in.** Add a *List this game publicly* checkbox to the room panel, off by default.
- **Listing message.** Send `RoomListing { room, host, seats, members }` as JSON inside a
  `UsernameWireMessage (Tuple listingJson ("listing/" <> code))` with header
  `dom/2/lobby/<sender>`.
  - Do **not** add `WireMessage` constructors, because that shifts the encoding of existing ones.
  - Do **not** add a new body type either, because pre-MVP clients would throw or misread it.
  - Pre-MVP clients only insert an unused username entry.
  - MVP clients drop `dom/2` messages unread.
  - Iteration-2 clients decode the listing through a `Prism' WireMessage RoomListing`.
- **Who announces.** Only the member who opted in, every heartbeat, while the room is
  unstarted. When that member leaves, the listing expires.
- **Soft-state lobby.** Global-room clients keep `HashMap RoomCode { listing, lastSeen }`
  and drop entries older than `heartbeatTimeout`.
- **UI.** An **Open games** list in Settings → Game (global room only), showing host,
  members/seats and a **Join** button that sets `#room=<code>`. Hide full rooms.

## Acceptance Criteria

- [ ] A listed room appears in every global-room client within one heartbeat, and
      disappears within `heartbeatTimeout` after the announcer leaves or starts the game.
- [ ] Pre-MVP and MVP clients are unaffected. Add wire tests in `test/Room.purs` like the
      existing "today's envelope decoder" tests.
- [ ] Property tests for the listing prism round trip and for lobby expiry.

## Later

- Failover: elect the announcer as the lowest id in **self ∪ live peers**, never peers
  alone, because `connectedClients` excludes self. This requires sharing the listing
  settings with every member first.
- Quick join: join the fullest open room, or create one if there is none.
