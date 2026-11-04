# Async Play: A Phone That Sleeps Doesn't Catch Up

**Source:** [Async play design](../../docs/2026-11-03-async-play-design.md) §6.4.11

**Status:** Closed (fixed on 2026-11-04 in v0.41.0)

## Description

Two players reached round 48 of a game. One phone was turned off while the other player
played on. When it was turned back on, it didn't catch all the way up to round 50, "and
other variations on that".

Cause:
- A phone that sleeps, or switches networks, can leave its WebSocket **half-open**: the
  browser still reports it open, no `close` event fires, so the reconnect logic never
  runs, but nothing is delivered either way.
- A client looked for news only on discrete events (startup, becoming visible, network
  back, relay back, a retry timer). On waking, the phone pulled once and caught up to
  that moment. Later moves went into its dead socket, and it never looked again while
  visible.
- The phone's own moves reached the mailbox, but their frames went into the same dead
  socket, so the other player never pulled them.
- The global room has no mailbox, so nothing replaced a half-open socket at all.

## Fix

The resync policy is a pure table in `Domination.Data.Resync`, from the moments a client
can notice to what it does then. Main only performs the reactions. New in v0.41.0:

- While the page is visible, poll the mailbox every 30 s.
- While the page is visible, reopen a socket that has heard nothing for 15 s, doubling
  up to 2 minutes while it stays silent.
- Reopen the socket when the network comes back, and on waking after 30 s or more hidden.
- Measure time with the wall clock, which keeps running while a phone sleeps.

## Acceptance Criteria

- [x] Property tests reproduce the report and three variations, in mailbox rooms and the
      global room, before the fix: v0.40.0's policy strands a device in each.
- [x] Under the new policy the same scenarios converge, random schedules converge within
      two polls (mailbox rooms) or the longest silence (global room), and nobody goes back
      to an older game.
- [x] A TLA+ model, `tla/RoomSync.tla`, finds liveness counterexamples for v0.40.0 in both
      kinds of room and no error for v0.41.0. `tla/check` runs it.
- [x] Browser test with a relay that makes a socket half-open: v0.40.0 fails in both
      kinds of room and v0.41.0 passes, after a short nap and a long one.
- [x] The version is v0.41.0.
