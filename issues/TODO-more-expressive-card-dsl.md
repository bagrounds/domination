# More expressive card effect DSL

**Source:** README TODO list

**Status:** Open

## Description

Expand the card effect Domain Specific Language (DSL) to support more complex and interesting card interactions. The current stack-based DSL (see `Data.Stack`, `Data.Stack.Machine`, `Data.Stack.Operations`) handles basic effects but could be more expressive.

## Potential Enhancements

- Conditional effects based on game state
- Effects that interact with multiple players simultaneously
- Delayed/triggered effects
- Effects that modify other cards
- Recursive or looping effects
- Better support for the reaction system (see `reactions-rebased` branch)
