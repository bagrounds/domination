# Browser Crash: Chat History Holds a Full Game per Play and Outgrows localStorage

**Source:** [#28 Repro Browser Crash](https://github.com/bagrounds/domination/issues/28),
found with `e2e/chaos.js --scenario turns`

**Status:** Closed (fixed on 2026-11-07 in v0.43.0) by R3 and R4a of the
[storage design](../../docs/2026-11-07-browser-crash-storage-design.md): plays are logged by
card name (`Domination.Data.Chat`), and `record` bounds the chat on every path. The chat is
still saved on every notice; at a few kB that's cheap, so it's not debounced.

## Description

The chat panel is also the game log. Each play adds a `PlayMadeMessage` to `_messages`,
and that message holds the full `Game` after the play (`src/Message.purs` 65–69). As JSON
that's about 50–80 kB per play. Nothing bounds it:

- Our own plays: `HandleGameEvent NewState` (`src/Main.purs` ~639) adds one with no cap.
- Games applied from the relay or mailbox: `applyGame` (~826) adds one with no cap.
- `take 250` only runs when a chat message arrives (~689) and in `notify` (~810). But 250
  plays × 50–80 kB is 12–20 MB, about three times the whole 5 MB localStorage quota.

Before async play, `saveChat` ran only when someone chatted, so a big log seldom touched
storage. Async play (`aedda41`) added `notify`, which saves the chat on every catch-up and
every not-applied notice. Now a long game writes megabytes to localStorage on almost every
return to the tab. Once the chat plus the 11 saved game states (~60 kB each), `debug_log`
and `sync_journal` pass the quota, every save fails. That sets off the
[storage error chat loop](./2026-11-07-browser-crash-storage-error-chat-loop.md), which
crashes the tab.

In `e2e/chaos.js --scenario turns` runs without filler, the chat key reached about 3.5 MB
at roughly game index 180–220, which is where the saves started to fail.

Every catch-up also stringifies and writes those megabytes on the main thread, which is
slow on phones even before the quota runs out.

## Approach (suggestions only)

- Keep the play in the log, not the whole game: what was played and by whom, plus only
  what `renderHtml` needs to show it.
- Bound the saved chat by size as well as by count, and apply the same bound on every
  path that adds a message.
- Save the chat only when a real chat message changes it, or debounce the save.

## Acceptance Criteria

- [x] A property test: after any sequence of plays, applied games, notices and chats, the
      encoded chat stays below a fixed size (100,000 characters plus the JSON separators).
- [x] A 120-turn `node chaos.js --scenario turns` run never shows `Error saving to local storage`.
