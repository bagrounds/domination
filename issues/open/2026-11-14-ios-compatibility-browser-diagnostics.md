# iOS Compatibility: In-App Browser Diagnostics

**Source:** [iPhone compatibility research](../../ai-blog/2026-11-14-iphone-compatibility-research.md)

**Status:** Open

## Description

When a player reports that Domination does not work in a particular browser,
there is currently no in-app way to check which required browser features are
available or behaving unexpectedly. Add an on-demand compatibility check that
runs in the player's browser and makes its results available in the existing
Debug report and prefilled GitHub bug report.

This is a diagnostic aid, not a way to certify a browser as compatible. Feature
presence checks cannot prove that the rendered layout, touch interactions, or
network behavior work on every device.

## Approach

- Add a user-invoked Compatibility check in Settings → Debug.
- Keep each check small, named, and feature-focused. Start with APIs and
  behavior the app relies on, such as storage access, WebSocket construction,
  secure-context crypto, audio availability, and CSS support for selected
  layout features.
- Use non-destructive checks: temporary storage keys must be removed, and the
  suite must not change a game, send a multiplayer message, or request
  permissions.
- Report results as pass, warning, or not checked, with the feature name and a
  concise failure reason. Preserve the browser and app version context already
  present in the debug report.
- Include the latest results in the copied report and GitHub issue diagnostics.
  The player reviews and submits the report; the app must not submit it
  automatically.
- Keep the check runnable on demand and avoid holding up game startup.

## Acceptance Criteria

- [ ] A player can run the compatibility check from Settings → Debug.
- [ ] Results identify each checked feature and distinguish failure from a
      feature that was not tested.
- [ ] A blocked or unavailable browser API produces a diagnostic result rather
      than crashing the game or the check.
- [ ] The results appear in both the copied debug report and the prefilled
      GitHub bug report.
- [ ] Checks do not alter game state, send network messages, request
      permissions, or submit a report.
- [ ] The UI explains that a passing check is not a guarantee of full browser
      compatibility.
