# Pseudo-random numbers

**Source:** README TODO list

**Status:** Open

## Description

Implement pseudo-random number generation with seeds for deterministic shuffling. This enables:

- **Undo support**: Deterministic shuffling means we can replay the game state forward from any point
- **Lightweight state diffs**: Instead of sending full game state, send only the actions taken (since shuffling is deterministic, both peers compute the same result)
- **Replay**: Games can be fully replayed from a seed and a sequence of actions

## Technical Notes

- Replace the current `Random` capability with a seeded PRNG
- Each game session gets a shared seed
- Actions become the only state that needs to be synchronized
