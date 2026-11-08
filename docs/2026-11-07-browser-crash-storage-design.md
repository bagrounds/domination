# Browser Crash: Bounded Chat, Logs and Storage Errors (Design)

- **Date:** 2026-11-07
- **Status:** Approved and implemented: R1, R2, R3, R4a, R5, R6. Not chosen: R4b (the
  alternative to R4a). Results are in §10.
- **Issue:** [#30 Fix: Browser Crash: Storage Errors Snowball Through Chat](https://github.com/bagrounds/domination/issues/30)
- **Bugs it addresses:**
  [storage error chat loop](../issues/closed/2026-11-07-browser-crash-storage-error-chat-loop.md),
  [chat history unbounded](../issues/closed/2026-11-07-browser-crash-chat-history-unbounded.md),
  and part of [instrumentation](../issues/open/2026-11-07-browser-crash-instrumentation.md)

## 0. How to Review This

Each recommendation has an ID (R1–R6) and is meant to be approved or rejected on its own.
§1 is the whole proposal on one screen; the later sections hold the detail for each row.
To approve, reply with the IDs, for example "go ahead with R1, R2, R3, R4a, R5".

## 1. Summary

The crash has two causes that feed each other:

1. **Errors go into the chat.** A failed save becomes a chat message that contains the
   value that failed to save, twice. The chat is then saved again and fails with a bigger
   value. Each round makes the next error several times larger, until the tab runs out of memory.
2. **The chat is huge.** Every play in the chat holds a whole copy of the game (about 48 kB
   of JSON for a fresh two-player game with the default kingdom). The chat outgrows the
   5 MB localStorage quota long before its `take 250` cap applies.

R1 and R2 break the loop, so the tab can't crash this way. R3 and R4 keep the chat small, so
saves stop failing in the first place. R5 bounds the debug logs by size.

| ID | Recommendation | Fixes | Depends on | Effort | Approve? |
| --- | --- | --- | --- | --- | --- |
| **R1** | Stop reporting errors in the chat. Send them to the console, where the debug log records them. | The loop | none | S | ✅ done |
| **R2** | Never put a stored value in an error message. Report the key, the size and the browser's error. | Multi-MB error strings | none | S | ✅ done |
| **R3** | Bound the chat by **size** (100,000 characters) as well as count (250), through one function every path uses. Cap each chat message at 500 characters. | Unbounded chat | R4, to be useful | S–M | ✅ done |
| **R4a** | Store **what was played**, not the whole game. Card names are looked up when the play is logged. *(recommended)* | Quota exhaustion | none | M | ✅ done |
| R4b | *Alternative to R4a:* store a pre-rendered plain-text line. Simpler, but loses the icons in the log. | Quota exhaustion | none | S–M | — not chosen |
| **R5** | Bound `debug_log` and `sync_journal` by **size**: 200,000 and 400,000 characters. | Debug logs taking 2 MB of quota | none | S | ✅ done |
| R6 | *Optional:* if persisting the debug log fails, drop its older half and try once more. | Debug log missing around a crash | R5 | S | ✅ done |

Effort: S is under about 50 lines plus tests, M is about 100–250 lines plus tests.

**Smallest set that stops the crash:** R1 + R2. Saves would still start failing in long
games, and the saved game would no longer be written (see §2.4), so I don't recommend
stopping there. **Recommended:** R1, R2, R3, R4a, R5.

Things I'm *not* proposing now are in §9, including dropping the duplicate game from the
wire format.

## 2. What Happens Today

All references are to `src/Main.purs` unless noted.

### 2.1 Every failed save goes into the chat

About 18 `save … >>= logErrorToChat` calls (game state, undo states, settings, mailbox sync,
version, chat) send their errors to `logErrorToChat` (~784):

```purescript
logErrorToChat result = case result of
  Left err -> do
    H.modify_ $ _message .~ ("ERROR: " <> err)
    sendChatMessage
```

`sendChatMessage` then:

1. puts the error into `_messages`, without the `take 250` cap;
2. calls `saveChat`, which writes the whole chat including the new error (this fails too,
   and that error goes to `console.error`);
3. **broadcasts** the error to the other players, who add it to their chat and save it,
   so their storage fills up as well.

It also overwrites the text box, so a chat message you were typing is lost.

### 2.2 Error messages contain the value, twice

- `setItem` in `src/FFI.js` (~251): ``left(`Error: setItem: key='${key}', value='${value}', …`)``
- `saveStorage` in `src/Capability/Storage.purs` (~64): ``"… value: '" <> stringValue <> "' error: '" <> error <> "'"``,
  where `error` is the first message, which already contains the value.

A failed 3.5 MB chat save produces a 7 MB error. That error goes into the chat, the next
save fails with a value of more than 10 MB, and so on. The crash runs recorded errors of 7, 27, 94
and 105 million characters.

### 2.3 The chat holds one game per play

See §3. The measurements below are for a freshly created game, before the decks grow:

| Chat entry (encoded JSON, characters) | 2 players | 4 players |
| --- | --- | --- |
| `PlayMadeMessage`, default kingdom (all 60 cards selected) | **47,812** | **53,089** |
| `PlayMadeMessage`, 17 cards in the supply | 13,699 | 18,971 |
| `ChatMessage` ("nice buy!") | 122 | 122 |

250 plays × 48 kB is about 12 million characters. Chrome's quota is 5,242,880 characters
per origin. Own plays (`HandleGameEvent NewState`, ~639) and applied games (`applyGame`,
~826) add entries with no cap. Since async play, `notify` (~807) saves the chat on every
catch-up.

### 2.4 Why R1 + R2 alone aren't enough

Once the chat fills the quota, **every** save fails, not only the chat. That includes
`game_state` and the undo slots. With R1 + R2 the tab stays alive, but the game is no
longer saved, so a reload or a discarded tab loses progress. To prevent that, the chat
has to stay small (R3 + R4).

### 2.5 The debug logs are bounded by count, not size

`src/DebugLog.js` keeps two ring buffers in localStorage, capped by entry count, with each
entry cut to 1000 characters:

| Key | Max entries | Max chars per entry | Worst case |
| --- | --- | --- | --- |
| `debug_log` | 400 | ~1,040 | ~416,000 |
| `sync_journal` | 1,500 | ~1,040 | **~1,560,000** |

Together they can take about 2 million characters, nearly 40% of the quota. These logs
are meant to help, but on a busy device they can be what fills the quota.

## 3. Why the Chat Stores the Full Game

**Yes, it's true.** Each play in the chat is a

```purescript
PlayMadeMessage { play :: Play, playerIndex :: Int, state :: Game }
```

(`src/Message.purs` 65–69). `state` is the game **before** the play: `src/UI/Domination.purs`
941–947 builds `playMade` from `activeState.state`, not from the new state.

**What it's for:** turning card *indices* into card *names* when the log line is rendered.
A `Play` refers to cards by position, and only the game at that moment knows which card
was there. `describePlay` (`src/Message.purs` 210–237) uses `state` in three places:

| Play | Rendered as | Needs from `state` |
| --- | --- | --- |
| `PlayCard { cardIndex }` | "played: Village" | the name of hand card `cardIndex` (`getPlayerCardName`) |
| `Purchase { stackIndex }` | "purchased: Gold" | the name of supply stack `stackIndex` |
| `ResolveChoice` with `MoveFromTo { resolution: Just indices }` | "trashed: Copper, Estate" | the names of those cards in the source pile (`getCardName` in `src/UI/RenderText.purs`) |
| `NewGame`, `React`, `DoneReacting`, other choices | | nothing |

Nothing else reads it. I searched `src/` and `test/`: the undo slots, the mailbox,
resync, bots and tests don't use `PlayMadeMessage.state`. It's never shown either. Since
it's the game *before* the play, it couldn't even show the board after a play.

A few more things I found along the way:

- `NewGame`'s `Play` also carries the whole supply (`supply :: Array Card`), and the log
  line only uses `playerCount`. So storing the `Play` as it is would still be large for
  that one entry. R4a handles it.
- The chat's element type is the wire type `RemoteMessage`, so `JoinMessage`,
  `HeartbeatMessage` and `GameMessage` could be put into the chat even though they never
  should be.
- Each `GameMessage` on the wire carries the game twice: `state` (after the play) and
  `playMade.state` (before it). See §9.

**Could we stop easily?** Yes. Look up the names when the play is logged and keep only
those. The change touches `Message.purs` (a new entry type), `RenderText.purs` (choice
rendering takes names instead of a `Game`), `UI/Chat.purs` and the five places in
`Main.purs` that add to the chat. **The wire format doesn't change**: a received
`GameMessage` still has `playMade.state`, and the receiver looks up the names on arrival,
as it renders them today.

**Pros**

- An entry is about 100 characters, or a few hundred for a choice, instead of about
  48,000. That's roughly 100–500 times smaller.
- 250 plays take tens of kB instead of about 12 MB, which removes the cause of the quota
  exhaustion.
- Saving on every catch-up becomes cheap. Today each catch-up stringifies megabytes on
  the main thread, which is slow on phones even before the quota runs out.
- Less memory: up to 250 games in `_messages` (no structure is shared after a reload) and
  faster parsing at startup.
- The types get tighter: the chat can only hold chat lines, notices and plays.
- A bounded history replay (the
  [history and chat issue](../issues/open/2026-11-03-async-play-history-and-chat.md))
  would add many plays to the log, which only works if entries are small.

**Cons**

- **No snapshot per play.** If we later want "click a log line to see the board then" or
  a replay, the chat can't provide it. A dedicated, bounded history would be the right
  place for that anyway. Today nothing uses the snapshot.
- **Saved chats need a migration** (§6.3). The old format still has the game, so
  converting uses the same function as new plays. Or we can drop the old chat once.
- **Rendering code changes.** Choice rendering takes the looked-up names instead of a
  `Game`, so a log line could come out slightly different. Tests in §8 compare old and
  new lines.
- Names are fixed when the play is logged. That's the same as now, since today's snapshot
  is also fixed at that moment.

## 4. R1: Stop Reporting Errors in the Chat

- Replace `logErrorToChat` with `reportSaveFailure`, which only calls `error`. The debug
  log already captures every `console.error` (`src/DebugLog.js` `captureConsole`), and
  errors also go to `sync_journal`, so they still appear in the bug report and the debug
  panel.
- `sendChatMessage` is only for chat the user typed. It no longer gets error text.
- Effects: the loop is gone; errors no longer reach other players; a message you're typing
  is no longer replaced by an error.
- *Optional sub-item R1b:* log a failing key once until it saves successfully again, so a
  full quota doesn't push the rest of the debug log out. With R2 and R5 each line is
  small and the log is bounded anyway, so I'd skip it unless we see it happen.

I'm not proposing a visible "storage is full" notice. If we want one later, it should be
UI state that is never saved, not a chat message.

## 5. R2: No Values in Error Messages

```text
Error saving to local storage. key: 'room/ab12/chat' (3,512,345 chars)
error: 'QuotaExceededError: Failed to execute 'setItem' on 'Storage': …'
```

- `src/FFI.js` `setItem` returns only `` `${e.name}: ${e.message}` ``, cut to 200
  characters. It no longer includes the value.
- `saveStorage` adds the key and `String.length stringValue`, not the value.
- The size of an error then depends only on the key, not on the value. A property test
  checks this (§8).
- This is also the first item of the
  [instrumentation issue](../issues/open/2026-11-07-browser-crash-instrumentation.md):
  log storage failures as key and size.

## 6. R3 + R4: A Small, Bounded Chat

### 6.1 R4a: store what was played (recommended)

A chat type of its own, separate from the wire type `RemoteMessage`:

```purescript
data ChatEntry
  = Said { sender :: String, text :: String, chatNumber :: Int }
  | Notice String                       -- caught up, not applied
  | Played { playerIndex :: Int, play :: LoggedPlay }

-- A play with every card index already looked up.
data LoggedPlay
  = CreatedGame { playerCount :: Int }
  | PlayedCard { card :: String }
  | Purchased { card :: String }
  | ResolvedChoice { choice :: Choice, cards :: Array String }
  | Reacted { reaction :: Maybe Reaction }
  | FinishedReacting

logPlay :: Game -> Play -> Maybe LoggedPlay   -- pure; EndPhase ↦ Nothing, as today
```

- `logPlay` takes over the lookups from `describePlay`. Rendering only needs the
  `LoggedPlay`, so the log keeps its icons and CSS classes.
- Received `ChatMessage`s become `Said` entries. `GameMessage.playMade` becomes `Played`
  via `logPlay playMade.state playMade.play`. Notices become `Notice` entries, not chat
  messages from a special sender.

### 6.2 R4b: store a plain-text line (alternative)

`Played { playerIndex :: Int, line :: String }`, rendered once when the play is logged.
It's the simplest storage format, but `RenderText` returns HTML with icons (the draw, action,
buy and money lines), so it needs a separate text renderer or the icons become words. I prefer R4a.

### 6.3 Migrating saved chats (part of R4)

The chat key stays `room/<room>/chat`. On load:

1. Decode it as `Array ChatEntry`.
2. If that fails, decode it as the old `Array RemoteMessage` and convert: `ChatMessage`
   becomes `Said`, `PlayMadeMessage` becomes `Played` via `logPlay` (the old entries
   still have their game), and anything else is dropped. Then apply R3's bound and save
   at once, which also frees the megabytes the old chat used.
3. If both fail, start with an empty chat, as today.

If you'd rather not keep the old decoder around, step 2 can be "start empty" instead. That
loses each device's old log once.

### 6.4 R3: one bounded way into the chat

```purescript
newtype Chat = Chat (Array ChatEntry)   -- newest first; constructor not exported

chatLimits = { maxEntries: 250, maxChars: 100000, maxTextChars: 500 }

record  :: ChatEntry -> Chat -> Chat     -- the only way to add
entries :: Chat -> Array ChatEntry

-- generic and pure, so it can be tested on its own
keepNewestWithin :: forall a. { maxCount :: Int, maxChars :: Int } -> (a -> Int) -> Array a -> Array a
keepNewestWithin { maxCount, maxChars } size =
  take maxCount <<< map fst <<< takeWhile (snd >>> (_ <= maxChars))
    <<< (\xs -> zip xs (scanl (+) 0 (size <$> xs)))   -- running total
```

- Because the constructor isn't exported, every path (own play, applied game, received
  chat, sent chat, notice, load) has to go through `record` or the decoder, which both
  apply the limits. Today `take 250` runs on two paths of five.
- An entry's size is the length of its encoded JSON. With R4 that's small, so measuring
  it on every add is cheap.
- `Said` text is cut to 500 characters by its smart constructor, so one message, local or
  received from someone else, can't use up the budget.
- **R3 without R4 doesn't help much:** with 48 kB entries, 100,000 characters holds only
  two plays.

Worst case for the saved chat: 250 entries at ~100 characters is about 25 kB, which is far
below the 100 kB budget and could never fill the quota.

Saving the chat on every catch-up (`notify`) can stay. With a chat of at most 100 kB it's
cheap, and leaving it keeps the change small.

## 7. R5 (+ R6): Debug Logs Bounded by Size

In `src/DebugLog.js`:

- Channels get a character budget as well as a count: `debug_log` 200,000 and
  `sync_journal` 400,000 characters. `append` keeps a running total per channel and drops
  the oldest entries while the total is over the budget. The existing 1000- and
  160-character limits per entry stay.
- At typical journal line lengths (~100–150 characters) this still keeps about the
  current 1500 entries. Only long lines are dropped sooner.
- Worst case for both logs: about 600,000 characters instead of about 1,976,000.
- **R6 (optional):** when `persist` fails (quota full), drop the older half of that
  channel and try once more, instead of silently giving up. This is the "keep room for
  the debug log" item of the instrumentation issue. It's what lets the log show what
  happened just before a crash.

Storage budget per origin after R1–R5 (worst case, characters):

| Key | Today | After |
| --- | --- | --- |
| `room/<room>/chat` | unbounded (3.5–4.2 M observed before failure) | ≤ 100 k |
| `debug_log` | ≤ ~416 k | ≤ 200 k |
| `sync_journal` | ≤ ~1,560 k | ≤ 400 k |
| `room/<room>/game_state` + 10 undo slots | ~11 × 48–80 k ≈ 0.5–0.9 M | unchanged (§9) |
| **Total, one room** | quota (5.24 M) reached in long games | ≈ 1.6 M |

## 8. Tests

The PureScript tests use QuickCheck (`test/Backoff.purs`, `test/LogicalClock.purs`).

| Test | Kind | For |
| --- | --- | --- |
| For any key and value, a failed save's error is at most `length key + 300` characters | property | R2 |
| For any sequence of `record`s (any entries, including very long chat text), the encoded chat is at most `maxChars + maxEntries + 2` characters and at most 250 entries, and the newest entry is kept | property | R3 |
| `keepNewestWithin` returns the longest prefix of newest entries within both limits | property | R3 |
| For any game in the tests' generators and any play, the encoded `Played` entry is at most 2,000 characters, regardless of game size | property | R4a |
| For each `Play` constructor, the new log line has the same text as `describePlay` (example-based, with a game built by `Game.new` and a few plays) | example | R4a |
| An old-format chat converts to entries with the same text (§6.3) | example | R4a |
| Debug log budget | manual, a node one-off; no JS test runner exists for `src/` | R5 |

End to end, from the bug's acceptance criteria:

```sh
npm ci && npm run bundle && cd e2e && npm ci
node chaos.js --scenario turns --seed 41 --steps 20 --fill-storage-kb 3500 --heap-mb 512   # must not crash
node chaos.js --scenario turns --seed 42 --steps 120                                        # no "Error saving to local storage"
```

## 9. Not in This Proposal

These came up while I investigated. Each is worth its own issue rather than being part of
this fix:

- **The game twice on the wire.** Every `GameMessage` carries `state` and
  `playMade.state`, which roughly doubles every relay frame and mailbox write. Sending
  the `LoggedPlay` instead would fix that, but it changes the wire encoding, and old
  clients would misread new frames (nested `Either` tags). It needs a protocol version
  step.
- **Saved games per room.** Each room keeps `game_state` plus 10 undo slots (~0.5–0.9 MB).
  Rooms are never evicted, so a device that has joined several rooms can fill the quota
  without any chat. Options include fewer undo slots, compressing stored games (the wire
  already uses LZString), or evicting rooms not used for a while.
- **The rest of the instrumentation issue:** quota usage per key in the bug report, a
  long-task observer, and a CI chaos job.

## 10. Results

Implemented in v0.43.0: R1, R2, R3, R4a, R5 and R6. R4b wasn't chosen.

### 10.1 What Changed

| ID | Where | What |
| --- | --- | --- |
| R1 | `src/Main.purs` | `logErrorToChat` is now `reportSaveFailure = either error pure`. A failed save goes to the console, and from there to the debug log. It no longer goes into the chat, isn't broadcast, and doesn't overwrite the text box. |
| R2 | `src/FFI.js`, `src/Capability/Storage.purs` | `setItem` returns only the browser's `name: message`. `saveFailureMessage` reports the key, the value's length and at most 200 characters of the browser's error. |
| R3 | `src/Data/Chat.purs` | `Chat` is a newtype whose constructor isn't exported. `record` and the decoder are the only ways to build one, and both keep at most 250 entries and 100,000 encoded characters, using `keepNewestWithin`. Chat and notice text is cut to 500 characters, and the sender to 100. |
| R4a | `src/Data/Chat.purs`, `src/UI/Chat.purs`, `src/Main.purs` | Plays are logged as `LoggedPlay`, built by the pure `logPlay :: Game -> Play -> Maybe LoggedPlay`, with card names looked up when the play is logged. Notices are `Notice` entries. The wire format is unchanged: a received `playMade` is logged the same way. |
| R4a migration | `src/Data/Chat.purs` (`DecodeJson Chat`), `src/Main.purs` `Initialize` | A chat saved in the old `RemoteMessage` format is converted when the room is opened (`fromRemoteMessage`), then bounded and saved straight away, which frees the space the old chat took. |
| R5 | `src/DebugLog.js` | `debug_log` and `sync_journal` keep a running character count and drop their oldest lines while over 200,000 or 400,000 characters, as well as over 400 or 1,500 lines. |
| R6 | `src/DebugLog.js` | When writing a buffer to localStorage fails, it drops its older half, adds a `WARN … localStorage is full, dropped the oldest N of M lines` line, and tries once more. It doesn't retry a failing write every second. |

Two limits differ from the plan in §6 and §8: the sender is cut to 100 characters too, and
the property test bounds a logged play at 4,000 characters rather than 2,000.

### 10.2 Tests

`test/Chat.purs` adds 13 tests, 7 of them QuickCheck properties:

- `keepNewestWithin` keeps the longest leading run within both limits.
- For any entries, the encoded chat stays within 250 entries and 100,251 characters
  (the 100,000 budget plus the commas and brackets).
- A chat filled past its limits still keeps the newest entry.
- Chat text and senders are cut to their limits.
- A logged play stays under 4,000 characters, whatever the size of the game.
- A chat survives encoding and decoding.
- A failed save's message is at most the key's length + 300 characters and never
  contains the value.
- Examples: `logPlay` names the card played, the card bought and the cards a choice moved
  (`???` for a missing one); it skips ending a phase; an old-format chat is read with
  plays logged by name; `recordPlay` ignores ending a phase.

To check that the size property catches a regression, I broke the size limit on purpose,
and it failed. The full suite passes: 515 of 515.

There's still no JS test runner for `src/`, so R5 and R6 were checked through the
browser runs below.

### 10.3 Size

In a browser check, the whole chat for a short session was 4,312 characters. Before, a
single play was about 48,000 (§2.3).

### 10.4 Crash Runs

`e2e/chaos.js --scenario turns`, the reproducer from the
[bug](../issues/closed/2026-11-07-browser-crash-storage-error-chat-loop.md):

| Run | Before | Now |
| --- | --- | --- |
| Seeds 41, 7 and 3, 20 turns, `--fill-storage-kb 3500 --heap-mb 512` | all three crashed in 47–65 s | all pass |
| Seed 42, 120 turns, `--fill-storage-kb 3500` | crashed; without filler it already crashed at 418 s with a 4,022 MB heap | passes: 1,598 plays, peak heap 54 MB, no save errors |
| Seed 43, 120 turns, no filler | crashed at 141 s | passes |

### 10.5 What the Harder Runs Found

I also tried harsher settings:

| Run | Result |
| --- | --- |
| `--scenario chaos --room mailbox --runs 3 --seed 100 --steps 300 --heap-mb 256 --fill-storage-kb 4800` | 2 of 3 runs (seeds 100 and 102) never converged |
| A long mailbox run, seed 200, 800 steps | passes |
| Two relay-only runs with `--fill-storage-kb 4000` | pass |

No tab crashed. The two failures are a different bug. With storage almost full, a
device's game save (about 50 kB) fails on the quota, but its small mailbox sync record
still saves, saying "up to date at revision N". After a reload the device has the
older game, the mailbox answers "unchanged", and the device stays a play behind
indefinitely. The short save errors from R2 made this easy to read in the journal.

A full quota isn't only a test setting. Nothing ever removes a room's saved data, and
each room keeps about 550 kB of saved games (§9), so about nine rooms fill the 5 MB
quota. That bug is filed, with evidence and suggested fixes, as
[a failed game save leaves a device a play behind](../issues/open/2026-11-07-async-play-stale-game-after-failed-save.md).
It isn't fixed here.
