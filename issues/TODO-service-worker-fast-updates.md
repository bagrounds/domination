# Service Worker: Deliver New Builds Faster

**Source:** [Matchmaking design](../docs/2026-10-31-matchmaking-design.md) §6.1, §7.2

**Status:** Open

## Description

`assets/sw.js` is cache-first and never calls `skipWaiting()` or `clients.claim()`, so
open tabs and installed PWAs can keep running an old build for a long time. That slows
every protocol rollout. The rooms MVP is wire-compatible, but old clients still mix
private-room games into the global room until they update (design §7.1).

The cache name is also out of sync with the app version: `CACHE_NAME = 'v0.37.0'` in
`assets/sw.js`, while `src/Version.purs` reports v0.37.1. `scripts/bump-version` keeps
them in sync, but only when it is used.

## Tasks

- [ ] Release the rooms MVP with `scripts/bump-version`, so the cache name changes and
      browsers fetch the new build.
- [ ] Call `self.skipWaiting()` in `install` and `clients.claim()` in `activate`, and
      delete caches that are not in `cacheAllowlist`.
- [ ] Optionally, show a "new version available, reload" notice when a new worker takes control.
