# Matchmaking: Retire the Shared Global Game

**Source:** [Matchmaking design](../docs/2026-10-31-matchmaking-design.md) §6.2, §7.2 (iteration 3, contract step)

**Status:** Open (blocked until pre-MVP clients are rare)

## Description

During the transition, clients from before rooms existed cannot filter by room. They
adopt games from every room and may rebroadcast them into the global room on startup
(design §7.1). Once those clients are rare, make the global room a **lobby only**:

- Starting a game always creates a room.
- The global room shows *Open games* and lobby chat.
- A dedicated `LobbyWireMessage` body becomes safe, because every client checks the
  header before decoding the body.

## Acceptance Criteria

- [ ] There is a way to tell that pre-MVP clients are rare, for example the version
      reported in the Debug tab, or a `dom/1` share of traffic seen in the global room.
- [ ] *Start game* in the global room creates a room and moves the player into it.
- [ ] The global room never holds a game state.
