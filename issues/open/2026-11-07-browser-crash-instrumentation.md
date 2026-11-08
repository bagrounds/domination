# Browser Crash: Instrumentation to Catch Memory and Storage Trouble Early

**Source:** [#28 Repro Browser Crash](https://github.com/bagrounds/domination/issues/28)

**Status:** Open. Partly done by the
[storage design](../../docs/2026-11-07-browser-crash-storage-design.md): failed saves now
log the key and value size to the console (and so to the debug log), never the value (R2);
the debug logs are bounded by size and halve themselves once when the quota is full (R5, R6).

## Description

We could only explain the crash in the
[storage error chat loop](../closed/2026-11-07-browser-crash-storage-error-chat-loop.md) once
`e2e/chaos.js` measured from outside the page (heap via the DevTools protocol, localStorage
by key, and the length of every console message). The app's own tools (`src/DebugLog.js`,
the bug report) were blind to it:

- **The debug log stops before the crash.** `persist` writes `debug_log` and
  `sync_journal` to the same full localStorage and swallows the error
  (`src/DebugLog.js` ~63–72). In the seed-31 crash, B's persisted log ends about 110 s before the tab died, just as
  the saves started to fail.
- **The first storage error is never logged.** `logErrorToChat` only puts it into chat.
  The console only sees the next, larger error, and the debug log cuts that down to
  1000 characters, so a 99 M-character error looks the same as a 2 kB one.
- **Nothing measures size.** No log line shows how big the chat, saved games or debug
  buffers are, how full the quota is, or how large the heap is.
- **Nothing shows a frozen main thread.** The tab was blocked for tens of seconds at a
  time before it crashed.

## Approach

Keep it small, and reuse the debug log:

- ~~Log storage failures as data: key, value length~~ (done, R2). Still to do: the total
  localStorage length, and a count of failures per key in memory.
- Have the bug report include localStorage usage per key (the top few) and
  `performance.memory` where available (Chrome).
- Record long tasks with a `PerformanceObserver` for `longtask` entries over about 1 s,
  in `sync_journal`.
- ~~Keep room for the debug log: if a persist fails, drop the oldest half of the buffers and
  try once more, and log that it happened~~ (done, R6).
- Put the size in the debug log's truncated lines, as `truncate` already does (`… (N chars)`),
  and record which log line was the largest.

For testing, as a small CI job:

- Run `cd e2e && node chaos.js --scenario turns --seed 41 --steps 20 --fill-storage-kb 3500 --heap-mb 512`
  on pull requests that touch `src/Main.purs`, `src/Capability/Storage.purs` or `e2e/`.
  It takes about 1.5 minutes and exits with 2 on a tab crash.
- Upload `e2e/runs/` as an artifact when it fails.

## Acceptance Criteria

- [ ] After a localStorage failure, the bug report names the key, the value size and the
      quota usage.
- [ ] A long main-thread stall shows up in `sync_journal`.
- [x] The debug log is still persisted when the quota is full (it halves itself and
      retries once; see `src/DebugLog.js`).
- [ ] The CI chaos job runs the turns scenario and keeps its journal on failure.
