# Async Play: A Failed Game Save Leaves a Device a Play Behind for Good

**Source:** an aggressive `e2e/chaos.js` run after the
[storage error chat loop](../closed/2026-11-07-browser-crash-storage-error-chat-loop.md) fix

**Status:** Open (reproduced, not fixed)

## Description

The saved game and the mailbox sync record are written to localStorage separately, and
nothing checks that both writes succeed. When storage is nearly full, the ~50 kB game
save can fail while the ~570-character sync record still fits. After a reload, the
device then has an older game but a sync record saying it already holds the newest
revision:

1. B plays. `saveGame` (`src/Main.purs` ~759) writes `game_state_<i mod 10>` and
   `game_state`. Both throw `QuotaExceededError`. The failure is logged, and the game
   continues in memory.
2. B's mailbox PUT succeeds (`Stored 9`). `setSync` (~795) saves
   `mailbox_sync = { confirmed: 9, dirty: Clean }`. It's small, so it fits.
3. B's tab is closed and reopened. `Initialize` restores the **older** `game_state`
   (i=11) and the sync record (confirmed 9).
4. B polls with `If-None-Match: "9"`. The mailbox answers `304 Unchanged`, and B keeps the
   older game. Nothing ever compares the restored game with revision 9, so B stays one
   play behind until someone plays again.

The other device (A, i=12) also reports `Clean`, confirmed 9, so neither device notices.

This isn't limited to a test harness. No code ever removes a localStorage key
(`grep removeItem src` finds nothing). Every room ever opened keeps its game and 10 undo
slots, about 550 kB per room, so roughly nine rooms fill the 5 MB quota, even with the
[bounded chat](../../docs/2026-11-07-browser-crash-storage-design.md). Old-format chats in
rooms not opened since the fix also keep up to ~3.5 MB each, until that room is opened
and its chat rewritten.

## Evidence

`cd e2e && node chaos.js --scenario chaos --room mailbox --runs 3 --seed 100 --steps 300 --heap-mb 256 --fill-storage-kb 4800`.
Two of the three runs failed the same way:

- seed 100: `no convergence within 70 s: A i=12 … confirmed 9; B i=11 … confirmed 9; mailbox revision 9`
- seed 102: `no convergence within 70 s: A i=10 … confirmed 11; B i=9 … confirmed 11; mailbox revision 11`

The harness compares the games the devices have **saved**. A page that never reloads can
still show the right game in memory. But its saved copy is stale, and that is what a
reload, a discarded tab or a closed app comes back to.

From seed 100's journal, `e2e/runs/chaos-mailbox-100/journal.txt`:

```
224.7 B console: Error saving to local storage. key: 'room/chaos100/game_state_2' (52926 chars) error: 'QuotaExceededError: …'
224.7 B console: Error saving to local storage. key: 'room/chaos100/game_state' (52926 chars) error: 'QuotaExceededError: …'
225.0 mailbox: PUT if-match="8" if-none-match=- -> 200 9
225.0 B console: Mailbox: PUT … -> (Stored 9)
237.7 B console: Initialize: load game...
237.7 mailbox: GET if-match=- if-none-match="9" -> 304 9
308.8 final: {"A":{"i":12,…"confirmed\":9}"},"B":{"i":11,…"confirmed\":9}"}}
```

With the storage fix, the error lines are short and name the key and size, which made
this easy to read. Before, each of them would have been a multi-megabyte chat message.

## Approach (suggestions only)

- **Keep the sync record honest.** Only save `confirmed: n` once the game it confirms is
  saved. If the game save fails, save the sync record as `Dirty`, or don't save it, so
  the next load asks the mailbox for the latest game instead of sending `If-None-Match`.
  Or store the revision inside the saved game, so the two can't disagree.
- **Write the important key first.** `saveGame` writes the undo slot before `game_state`.
  Writing `game_state` first gives the game the space that is left.
- **Make room when the quota is full.** On `QuotaExceededError`, drop this room's oldest
  undo slots, then other rooms' saved data (oldest first), and try once more. That needs
  a small index of rooms and when each was last opened.

## Acceptance Criteria

- [ ] A model test (as in `test/Resync.purs`): if a game save fails, a reload never
      leaves a device with an older game and a `Clean` sync record for a newer revision.
- [ ] `node chaos.js --scenario chaos --room mailbox --runs 3 --seed 100 --steps 300 --heap-mb 256 --fill-storage-kb 4800`
      converges on every run.
