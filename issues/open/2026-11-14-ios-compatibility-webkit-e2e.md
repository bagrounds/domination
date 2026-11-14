# iOS Compatibility: Playwright WebKit Coverage

**Source:** [iPhone compatibility research](../../ai-blog/2026-11-14-iphone-compatibility-research.md)

**Status:** Open

## Description

The existing browser chaos harness in `e2e/` launches Chromium. Add a WebKit run
so browser CI can catch engine-specific failures in the game and its multiplayer
recovery before users report them.

Playwright's WebKit is useful coverage, but it is not the branded Safari app or
an iPhone. Keep that distinction clear in test output and documentation.

## Approach

- Reuse the current local app, relay and mailbox fixtures.
- Allow the browser engine to be selected, or add a focused WebKit smoke test
  without duplicating the browser setup.
- Start with a short scenario: load a room, make a move, interrupt the page or
  connection, return, and verify that both players converge on the latest state.
- Keep the existing Chromium chaos coverage; add WebKit installation and the
  smoke test to CI.
- Preserve the run journal and browser console output when a test fails.

## Acceptance Criteria

- [ ] A WebKit browser run exercises the bundled app against local servers.
- [ ] The run verifies a game action and successful catch-up after an
      interruption.
- [ ] CI installs the matching WebKit browser and runs the smoke test.
- [ ] The existing Chromium chaos scenarios continue to run unchanged.
- [ ] Documentation says WebKit coverage is not equivalent to testing released
      iOS Safari on an iPhone.
