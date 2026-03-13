# Game AI players

**Source:** README TODO list

**Status:** Open

## Description

Implement AI players that can play the game automatically. The `makeAutoPlay` function in `Data.Game.Engine` already provides a foundation for automated gameplay, but a proper AI system would need strategy and decision-making.

## Potential Approaches

- **Rule-based AI**: Simple heuristics (e.g., buy the most expensive card you can afford)
- **Monte Carlo Tree Search**: Simulate many possible futures and pick the best action
- **Trained model**: Use reinforcement learning to train an agent
- **Difficulty levels**: Multiple AI strategies from "easy" to "expert"

## Existing Foundation

The `makeAutoPlay` function already demonstrates automated play. The existing `Choice` and `ResolveChoice` modules define the decision points where AI would need to make choices.
