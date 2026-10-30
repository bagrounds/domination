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

## Takeaways

Property-based and full-game simulations paid for themselves: each of the bugs above was found by bots playing thousands of turns, not by hand testing.
