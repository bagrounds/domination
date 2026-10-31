# Matchmaking: Catch-Up on Join

**Source:** [Matchmaking design](../docs/2026-10-31-matchmaking-design.md) §6.3

**Status:** Open

## Description

A guest who opens an invite link after the game has started sees nothing until the next
move. A member who reloads also stays on their (possibly stale) saved game until the
next move, because rooms deliberately do not broadcast saved games on startup.

When a `JoinMessage` is admitted in a game room, exactly one member that has a game
should resend its latest `GameMessage`.

## Approach

- Choose the responder as the lowest client id in **self ∪ live peers that have a game**.
  `connectedClients` excludes the local client, so electing from peers alone would leave
  a two-member room with no responder.
- Only the global room keeps today's share-on-startup behaviour
  (`sharesSavedGameOnStartup` in `src/Main.purs`). Leave it unchanged.
- Never answer with an older `i` than the joiner might already have. Note that `Undo`
  legitimately loads older states, so receivers must not reject older `i` in general.

## Acceptance Criteria

- [ ] A late joiner sees the current game within one round trip.
- [ ] Exactly one member responds per join (pure election function, property tested).
- [ ] No behaviour change in the global room.
