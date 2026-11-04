# Async Play: Leaving the App Leaves the Socket Stopped

**Source:** [Async play design](../../docs/2026-11-03-async-play-design.md) §6.4.12

**Status:** Closed (fixed on 2026-11-04 in v0.42.0)

## Description

They played a while, she left the app, he kept playing, and when she came back her game
was stuck behind his.

Cause:
- On `pagehide` the connection closes its socket and stops reconnecting. Only `pageshow`
  restarted it.
- A mobile browser may fire `pagehide` when the user leaves the app and later show the
  same page again without the back-forward cache and without `pageshow`. That page kept a
  stopped socket for good.
- Main was never told, so it believed the socket was open. In the global room, with no
  mailbox to poll, the device never caught up.

## Fix

- Any sign of life restarts a stopped connection: `pageshow`, the page becoming visible,
  a `resume` after a freeze, or a resync `reopen`.
- Stopping reports the relay lost.
- Bug reports include a sync journal and page facts, saved in `localStorage`.

## Acceptance Criteria

- [x] A model test reproduces the report: v0.41.0's connection strands the room without a
      mailbox.
- [x] With the fix, the story passes in both kinds of room.
- [x] A browser chaos harness in `e2e/` can play the report (`--scenario away --leave
      pagehide`) and random chaos against local servers it can break.
- [x] The version is v0.42.0.
