# Matchmaking

**Source:** README TODO list

**Status:** In progress (MVP implemented)

## Description

Implement a matchmaking system so players can find each other without sharing room codes manually. Currently, players must coordinate out-of-band to join the same game session via the peer-to-peer networking layer (Bugout/WebTorrent/WebRTC).

## Potential Approaches

- Lobby system with a signaling server
- Random matchmaking queue
- Public game rooms browser

## Design

See the proposal [`docs/2026-10-31-matchmaking-design.md`](../docs/2026-10-31-matchmaking-design.md):
invite-link rooms filtered in the client (MVP), then discovery of unstarted games through room listings in the global room.

## Progress

- [x] Iteration 1 (MVP): invite-link rooms. Create a private game, share the link, and
      play isolated from the global room (`src/Data/Room.purs`).
- [ ] [Discover and join unstarted games](TODO-matchmaking-discover-open-games.md)
- [ ] [Catch-up on join](TODO-matchmaking-catch-up-on-join.md)
- [ ] [Seat claiming](TODO-matchmaking-seat-claiming.md)
- [ ] [Retire the shared global game](TODO-matchmaking-retire-global-game.md)
- [ ] [Service worker: deliver new builds faster](TODO-service-worker-fast-updates.md)
- [ ] [Optional topic routing on the relay](TODO-matchmaking-server-topic-routing.md)
