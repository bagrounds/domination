# Browser Crash: Choice "Done" Buttons Accept Invalid Selections Without Feedback

**Source:** [#28 Repro Browser Crash](https://github.com/bagrounds/domination/issues/28),
found with `e2e/chaos.js --scenario turns`

**Status:** Open (low priority; not related to the crash)

## Description

The "Done" button of every choice dialogue (`.resolve-choice` in `src/UI/CardChooser.purs`
~117, `src/UI/ChooseCards.purs` ~85 and `src/UI/ChooseFromSupply.purs` ~69) is always
enabled. If the selection doesn't satisfy the choice, the engine rejects it and logs a
console error, and the player sees nothing happen. For example:

- `MoveFromTo` with `n: Exactly 1` and nothing selected:
  `(must have exactly 1 selected cards OR (must have less than 1 source cards AND must have exactly 7 selected cards))`
  (`Constraint.check` in `src/Data/Constraint.purs` ~51).
- `GainCard` with nothing chosen:
  `this is an unresolved choice: (GainCard { … resolution: Nothing })`
  (`src/Data/Game/ResolveChoice.purs` ~170).

Random play hit these 40–60 times per long `turns` run. They're probably the "caught
choice-validation error" lines in the original crash report's debug log. They are caught,
so they are not part of the crash, but they show up in every bug report and make the
real problem harder to find.

## Approach (suggestions only)

- Disable "Done" (`HP.disabled`, as `src/UI/Rename.purs` does) until the selection
  satisfies the choice, using the same validation the engine applies.
- Or show the validation message in the dialogue.

## Acceptance Criteria

- [ ] "Done" is only enabled for selections the engine accepts (a property test over
      random choices and selections).
- [ ] A long `node chaos.js --scenario turns` run logs no choice-validation errors.
