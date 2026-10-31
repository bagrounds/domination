# End-to-End Browser Tests for Private Game Links

**Source:** PR review of the rooms MVP; [Matchmaking design](../docs/2026-10-31-matchmaking-design.md) §10, §12

**Status:** Open

## Description

The repository has no browser-driven test suite. `npm run test` (`spago test`) covers
pure code only: QuickCheck properties in `test/Main.purs` and `test/Room.purs`. The
behaviour that matters most for private games happens across several browsers talking
through the relay, and it was checked only by hand (design §12): three tabs on a local
relay, plus one tab running an older build.

Add an end-to-end suite that drives real browsers against a real relay, and express its
checks as **properties over random action sequences** rather than a few fixed scripts.

## Approach

- **Runner.** [Playwright Test](https://playwright.dev/docs/intro) (`@playwright/test`)
  as a dev dependency. One `BrowserContext` per player, so each has its own
  `localStorage`.
- **Generators.** [fast-check](https://fast-check.dev/) model-based testing
  (`fc.commands`, `fc.asyncModelRun`). Commands: create room, open invite link, open a
  pasted or upper-case link, open an invalid link, send chat, start game, reload, leave
  room. Shrinking reduces a failure to the shortest sequence that reproduces it.
- **Model.** A pure map from player to room. Each command updates the model, and the
  checks compare every browser's UI to it.
- **Fixtures.** Follow the manual recipe from this PR:
  - start `node server/index.js` on a free port (`PORT`);
  - bundle with `spago bundle-app --main Main --to <dir>/dist/app.js` and serve `assets/`
    next to it, without `sw.js` (cache-first caching hides reloads);
  - before the app loads, set `localStorage["server-url"]` to
    `JSON.stringify("ws://localhost:<port>")`.
- **Wire tap.** A test-only WebSocket client on the relay records every frame, so tests
  can check what was sent and not only what was displayed.

## Properties

1. **Isolation.** A chat message or game shows up exactly in the browsers whose model
   room equals the sender's room.
2. **Invite round trip.** Opening the link copied from the Room panel puts the guest in
   the host's room, and the panel's "others here" count matches the model.
3. **Normalization.** A link that differs only by letter case joins the same room.
4. **Invalid links.** Any `#room=` value outside `[a-z0-9]{1,32}` lands in the global
   room and shows the notice.
5. **No rollback on reload.** A reload inside a room sends no `GameMessage` (wire tap),
   and the reloaded browser restores its own saved game.
6. **Leave.** After *Leave room*, the browser is in the global room and receives none of
   its old room's traffic.
7. **Old builds (optional).** A browser running the previous release chats with new
   clients in the global room, and clients in a private room never show its traffic.

## Tasks

- [ ] Add `@playwright/test` and `fast-check` as dev dependencies (check the advisory
      database first) and an `npm run test:e2e` script.
- [ ] Write fixtures for the relay, the static server and per-player contexts.
- [ ] Write the model, the commands and properties 1–6.
- [ ] Add property 7 with a pinned older build.
- [ ] Run the suite in `.github/workflows/ci.yml` after `npm run test`, with
      `npx playwright install --with-deps chromium`.
- [ ] Document the command in the README's Test section.
