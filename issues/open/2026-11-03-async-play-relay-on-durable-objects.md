# Async Play: Relay on Durable Objects

**Source:** [Async play design](../docs/2026-11-03-async-play-design.md) §6.7

**Status:** Open (later, optional)

## Description

The live WebSocket relay runs on Render's free plan. It sleeps after 15 minutes without
traffic and takes about a minute to wake up, and it sends every message to every client.
The room mailbox's Durable Objects could also carry the live messages.

## Approach

- The room's Durable Object accepts WebSockets with the hibernation API and broadcasts
  only within the room. This gives the per-room routing from
  [server topic routing](./2026-10-31-matchmaking-server-topic-routing.md).
- Trim heartbeat traffic first. Every heartbeat currently makes every peer reply with a
  `UsernameMessage`, and incoming WebSocket messages count against the free request quota
  (20 messages count as one request).
- Wait until [service worker fast updates](./2026-03-11-service-worker-fast-updates.md)
  make old builds rare, because old builds would stay on the Render relay.

## Acceptance Criteria

- [ ] Live play has no cold start.
- [ ] Clients only receive their own room's messages.
- [ ] Old and new builds can still play together during the migration, or the cut-over is
      announced.
