# Finishing the Bot Players: Undo-Friendly Turns, Tidy Settings, and a Supply That Remembers

The bot-player PR sat unmerged for months. Coming back to it, the job was to finish what was promised and harden it for production. Strict simulations turned up several real engine bugs, which also needed fixing.

## What the owner asked for

1. **Bot settings survive a reload.** The roster (bot strategies, player count and seat) and the bot delay are saved in localStorage. On startup, `Initialize` normalizes the roster, loads the game, then configures the bots, so bots never act on the placeholder game.
2. **Undo works with bots.** A bot no longer plays straight away. Its move is *scheduled*: a floating overlay shows exactly what it is about to do, with an SVG countdown ring. Undo during the countdown cancels the pending play. The delay (0–5 s) is set in Settings.
3. **Settings are organized into tabs:** Game, Bots, Kingdom, Connection and Debug.
4. **Bug reports from mobile.** A small FFI module captures console output and uncaught errors in a 400-entry ring buffer and saves it to localStorage. The Debug tab can copy a report with the version, user agent and timestamp, or clear the log. A read-only textarea is the fallback for iOS clipboard quirks.

## Design notes

- **Separating persisted from ephemeral state.** `ActiveState` (what gets saved) no longer contains bots. A `ComponentState` wraps it with the bot config, the pending play and the fork id of the timer. Saves keep the same shape as master, so older saves load again.
- **Generate and test.** The AI produces candidate plays and keeps the first one that a silent dry run of the real engine accepts *and* that changes the game. The bot can't make an illegal play, and the engine stays the single source of truth for the rules.
- **Declarative Big Money.** The buy rule is a priority table (Colony, Platinum, Province, Gold, Duchy, Silver, Estate) with greening once the Provinces run out.

## Bugs the simulations surfaced

Running many full bot games under strict invariants found engine bugs that affected human players too:

- **Remodel, Expand and Mine** read the cost *after* trashing the card, and Mine could soft-lock with no Treasure in hand. They now share one `trashFromHandToGainUpgrade` program.
- **Moat**: after a block, the reaction window stayed open even when no attack remained.
- **Lurker deleted empty supply piles.** The supply was edited by flattening it into cards and regrouping them, and any pile with zero cards disappeared. A later Witch then failed with "card not in supply!", and the end-of-game checks for empty piles could no longer see them. The supply now goes through a `_restocked` lens: reads are unchanged, and writes restore counts onto the original piles, so empty piles keep their place at zero. A QuickCheck property and an end-to-end Lurker → Witch test lock this in.

## Bugs only a browser could find

Clicking through the built app turned up two problems the unit tests couldn't see:

- **The delay slider showed 0 s while its label said 2 s.** Halogen sets the input's `value` before `min`, `max` and `step`. The browser clamped 2000 to the default maximum of 100, and the 250 ms step then snapped it to 0. Setting `value` last fixes it.
- **The countdown ring wasn't styled.** `HP.class_` sets the DOM `className` property, which is read-only on SVG elements, so the class was silently dropped. The SVG now sets `class` as an attribute.

## Takeaways

Property-based and full-game simulations paid for themselves: each of the bugs above was found by bots playing thousands of turns, not by hand testing.

## Follow-up: a test command that can fail, and two choice bugs

### The test command now fails when tests fail

The test runner printed `Results: N/M passed` but always exited 0, so `npm test` (and CI) passed even when tests failed. The runner now throws after printing the summary whenever anything failed, so the process exits non-zero.

### Card choosers now show cards in the order the engine counts them

When the engine resolves a choice like "pick a card from your discard pile", it reads each pile *grouped by card name*. The choosers, though, showed the raw pile. Picking the second card of `[Copper, Estate, Copper]` displayed an Estate but moved a Copper. Hands happened to stay grouped, which hid the bug; discard, deck, trash and in-play piles did not.

There is now a single source of truth, `Game.pileCards`, for "the cards of this pile, in the order the engine indexes them". The bots and all three chooser components use it. A randomized test plays 200 random discard piles and checks that choosing index *i* moves exactly the card `pileCards` shows at position *i*.

### Moat blocks the attack, not whatever choice comes first

Reacting with Secret Chamber puts its own choice *in front of* the attack. Blocking with Moat used to remove the first pending choice. If the block arrived before the Secret Chamber choice was resolved, Moat discarded the Secret Chamber choice and left the attack in place. The standard UI resolves the Secret Chamber choice first, so this mostly affected plays sent straight to the engine, such as bots or remote clients. `BlockAttack` now removes the first *attack* choice and is rejected when no attack is pending. Both cases have regression tests, and both fail against the old code.

Version bumped to **v0.37.0**.
