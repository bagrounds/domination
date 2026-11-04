# Async Play: A Device That Rejoins Rewinds the Game

**Source:** [Async play design](../../docs/2026-11-03-async-play-design.md) §6.4.10

**Status:** Closed (fixed on 2026-11-04 in v0.40.0)

## Description

Playing one seat on a phone and a laptop: close the laptop's tab, keep playing on the
phone, then reopen the laptop. The laptop's old game replaced the phone's. Whoever is
ahead should bring the other up to date.

A browser test with two profiles on one seat reproduced it in the **global room**. Rooms
with a mailbox passed.

Cause:
- The global room has no mailbox. On startup it broadcast its saved game, however old,
  and receivers took every game frame.
- Nobody sent the returning device the current game, so it stayed on its old one. Its
  next move, built on that game, rolled the others back as well.

## Fix

Versioned games, a Lamport clock in the pure module `Domination.Data.LogicalClock`, used
wherever there is no mailbox:

- Every change (play, undo, new game, loaded game) gets a version one later than the
  newest the device has seen. Versions ride in the header's sender segment, which old
  builds ignore.
- Receivers take only games ahead of their own and answer older ones with theirs.
- Whoever joins, and whoever sees them join, offers their game. The blind share on
  startup is gone.
- Games saved before v0.40.0 start at their move counter.

The version is now v0.40.0, so devices load the new build.

## Acceptance Criteria

- [x] A test reproduces the report: under the old rule the phone is rolled back.
- [x] With versions the phone keeps its game and the returning laptop catches up, and
      the other way round.
- [x] Undo and New Game still reach everyone.
- [x] Property tests: nobody takes a game behind their own, versions never go down, and
      everyone ends on the newest game.
- [x] Browser test of the reported story passes in the global room and in mailbox rooms,
      on the local and production relays, and from v0.39.0 saves.
