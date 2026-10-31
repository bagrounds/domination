# Service Worker: Deliver New Builds Faster

**Source:** [Matchmaking design](../docs/2026-10-31-matchmaking-design.md) §6.1, §7.2

**Status:** Open

## Description

`assets/sw.js` is cache-first and never calls `skipWaiting()` or `clients.claim()`, so
open tabs and installed PWAs can keep running an old build for a long time. That slows
every protocol rollout. The rooms MVP is wire-compatible, but old clients still mix
private-room games into the global room until they update (design §7.1).

A new build only reaches a browser when `sw.js` changes, which `scripts/bump-version`
ensures by renaming `CACHE_NAME`. Even then, the new worker waits until every tab using
the old one has closed.

## Tasks

- [x] Release the rooms MVP as v0.38.0, so `CACHE_NAME` matches `src/Version.purs` again
      and browsers install the new worker.
- [ ] Call `self.skipWaiting()` in `install` and `clients.claim()` in `activate`. The
      existing `activate` handler already deletes caches outside `cacheAllowlist`.
- [ ] Optionally, show a "new version available, reload" notice when a new worker takes control.
