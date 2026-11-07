# Browser Crash: Storage Errors Snowball Through Chat Until the Tab Runs Out of Memory

**Source:** [#28 Repro Browser Crash](https://github.com/bagrounds/domination/issues/28),
reproduced with `e2e/chaos.js --scenario turns`

**Status:** Open (reproduced, not fixed)

## Description

In a long game, the room's chat grows until saving it overflows the browser's localStorage
quota (Chrome allows 5,242,880 characters per origin). From then on every save error is
fed back into the chat, and each round makes the error larger than the last. Within a few
catch-ups the tab is handling strings tens of millions of characters long. It stalls for
tens of seconds, then the renderer runs out of memory and Chrome kills the tab.

The loop, all in `src/Main.purs` unless noted:

1. `saveChat` (~264) writes the whole `_messages` array to `room/<room>/chat`. Since async
   play, `notify` (~807) calls it on every catch-up (`caughtUpNotice`) and on every
   not-applied notice. Before, it ran only for chat. See the
   [chat history issue](./2026-11-07-browser-crash-chat-history-unbounded.md) for why the
   chat is so big.
2. Once the chat is too big, `localStorage.setItem` throws. The error text contains the
   whole value **twice**: once in `setItem` in `src/FFI.js` (~251–257, `value='…'`) and
   once more in `saveStorage` in `src/Capability/Storage.purs` (64–72, `value: '…'`).
3. `logErrorToChat` (~784) makes that text a chat message (`"ERROR: " <> err`) and calls
   `sendChatMessage` (~771). That adds the message to `_messages` with no cap, tries
   `saveChat` again (this fails too, and the new error, which now contains the previous
   one JSON-escaped, goes to `console.error`), and broadcasts the message over the relay.
4. The next `saveChat` from anywhere (another catch-up, a received chat, another
   `logErrorToChat`) fails with a bigger value, and the cycle repeats.

The first error never reaches the console. Only the second-generation error does, already
several times the size of the chat.

[Issue 13](../closed/2026-03-11-remove-infinite-error-chat-loop.md) removed the direct
recursion in `sendChatMessage`. This growth goes through state instead, so that fix didn't
stop it.

## Evidence

Every run of `node chaos.js --scenario turns` long enough to fill the quota crashed a tab.
The renderer's `crash` event fired and the harness exited with code 2. The sizes below are
`console.error` lengths from the journal.

| Run | Options | First visible error | Error sizes (M chars) | Crash | Peak heap |
| --- | --- | --- | --- | --- | --- |
| seed 31, 80 turns | none | t=322 s, i≈222 | not recorded (older harness) | B, t=422 s, turn 63 | 3984 MB |
| seed 41, 40 turns | `--fill-storage-kb 3500` | t=62 s | B 7.4, 26.7, 6.3, 27.0; A 6.4, 24.8, 93.8 | A, t=367 s, turn 30 | A 4014, B 3564 MB |
| seed 42, 120 turns | none | t=253 s, i≈180 | A 28.0, 99.5 | A, t=418 s, turn 62 | A 4022 MB |
| seed 41, 20 turns | `--fill-storage-kb 3500 --heap-mb 512` | t=71 s | A 7.0 | A, t=81 s, turn 18 | A 478 MB |
| seeds 41, 7, 3, 20 turns (fixed click rules) | `--fill-storage-kb 3500 --heap-mb 512` | t=32–54 s | 6.7–8.3 | 3 of 3, t=47–65 s | ≤ 497 MB |
| seed 43, 120 turns (fixed click rules) | `--heap-mb 512` | t=130 s, i≈140 | A 29.8 | A, t=141 s, turn 60 | A 509 MB |

- Without filler the chat key alone reached about 3.5 MB before the first failure
  (seed 42: `room/chaos42/chat 3487 kB`).
- The main thread stalls while one of these errors is handled. On seed 41, B's heap jumped
  from 383 MB to 1013 MB within one probe interval after the 7.4 M-char error. During the
  later errors, probes stopped for about 40 s and then all completed at the same instant.
- In seed 42, B's peak heap was only 65 MB while A crashed. So one device is enough, and
  the relay echo to the peer isn't needed. On seed 41 both devices took part.
- Closing the tab and reopening it resets the in-memory chat to the last chat that was
  saved successfully, so it takes a few rounds to reach the crash.
- With Chrome's default desktop heap (~4 GB) the crash came 100–170 s after the first
  visible error. With a phone-sized 512 MB heap it came 10 s after.

Caveat: Playwright attaches DevTools, which keeps console messages, so retained memory is
probably higher in the test than on a real phone. But the chat itself also keeps every
error message in `_messages`, so it grows on a phone too.

## Reproduce

```sh
npm ci && npm run bundle
cd e2e && npm ci
node chaos.js --scenario turns --seed 41 --steps 20 --fill-storage-kb 3500 --heap-mb 512   # ~1 min
node chaos.js --scenario turns --seed 42 --steps 120                                        # ~7 min, no shortcuts
```

The journal is in `e2e/runs/turns-mailbox-<seed>/journal.txt`. The devices' persisted
`debug_log` and `sync_journal`, read from a fresh page after the crash, are next to it.

## Approach (suggestions only)

- Never put a stored value in an error message. Say the key and the size instead.
- Don't report storage errors through chat. Use the console and the debug log, and maybe a
  one-time notice that doesn't get saved itself.
- Make saving the chat best-effort and bounded: cap the saved chat by characters, not only
  by count, and drop the oldest messages until it fits.

## Acceptance Criteria

- [ ] A property test: for any chat and any sequence of failing saves, the chat and every
      error message stay below a fixed size.
- [ ] `node chaos.js --scenario turns --seed 41 --steps 20 --fill-storage-kb 3500 --heap-mb 512`
      runs without a crash, and so does a 120-turn run without filler.
