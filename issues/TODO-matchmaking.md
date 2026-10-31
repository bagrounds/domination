# Matchmaking

**Source:** README TODO list

**Status:** Open

## Description

Implement a matchmaking system so players can find each other without sharing room codes manually. Currently, players must coordinate out-of-band to join the same game session via the peer-to-peer networking layer (Bugout/WebTorrent/WebRTC).

## Potential Approaches

- Lobby system with a signaling server
- Random matchmaking queue
- Public game rooms browser

## Design

See the proposal [`docs/2026-10-31-matchmaking-design.md`](../docs/2026-10-31-matchmaking-design.md):
invite-link rooms filtered in the client (MVP), then discovery of unstarted games through room listings in the global room.
