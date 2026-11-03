# Async Play

**Source:** [#22 Async Play](https://github.com/bagrounds/domination/issues/22),
[Async play design](../docs/2026-11-03-async-play-design.md)

**Status:** Open (designed, not implemented)

## Description

A game only moves forward when everyone is connected at the same time. A player who is
offline when a move is made never receives it. Someone has to press *Load Game* later to
push the state around again.

Players should be able to play at their own pace. When I make a move, you get it the next
time you open the game, even if I have closed my tab and the relay has restarted.

## Plan

The design recommends a content-agnostic **room mailbox**: a Cloudflare Worker with one
Durable Object per game room. It keeps the latest game envelope and accepts only
conditional writes. The WebSocket relay does not change.

1. [Reconnect sends](./2026-11-03-async-play-reconnect-sends.md): a prerequisite bug fix
   that is client only.
2. [Room mailbox](./2026-11-03-async-play-mailbox.md): async play itself.
3. Later, optional:
   - [History and async chat](./2026-11-03-async-play-history-and-chat.md)
   - [Turn notifications](./2026-11-03-async-play-turn-notifications.md)
   - [Relay on Durable Objects](./2026-11-03-async-play-relay-on-durable-objects.md)

Related:
- [Catch-up on join](./2026-10-31-matchmaking-catch-up-on-join.md) heals missed messages
  between peers who are online at the same time.
- [Reliable message passing](./2026-03-11-reliable-message-passing.md)

## Acceptance Criteria

- [ ] With the relay restarted and the mover's tab closed, a player who opens a game room
      sees the latest move.
- [ ] A move made while offline is delivered when the player is back online, even after a
      reload.
- [ ] A stale client cannot overwrite a newer move.
- [ ] Live play between old and new builds keeps working.
- [ ] Everything runs on free plans.
