# More thorough tests

**Source:** README TODO list

**Status:** Open

## Description

Expand the test suite to cover more game scenarios and edge cases. Current tests primarily cover:

- Stack machine computation
- Wire serialization roundtrips for game state
- Wire format isomorphisms for Game and Play types

## Areas Needing Coverage

- Individual card effect execution
- Game engine state transitions (action phase, buy phase, cleanup)
- Player state management (hand, deck, discard)
- Supply management (empty stacks, game end conditions)
- Choice resolution (card selection, supply selection)
- Multi-player interactions (attacks, reactions)
- Edge cases (empty decks, no valid actions, game over conditions)
- The normalized game state model (`NormalGame.purs`)
- Reaction system (for the `reactions-rebased` branch work)
