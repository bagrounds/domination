# Async Play: Turn Notifications

**Source:** [Async play design](../../docs/2026-11-03-async-play-design.md) §6.6

**Status:** Open (after the room mailbox)

## Description

With async play, a player does not know when it is their turn unless the tab is open.
Notify them.

## Approach

- Opt-in Web Push per room. The service worker subscribes with the Worker's VAPID public
  key and stores the subscription in the room's Durable Object.
- After an accepted `PUT`, the Durable Object sends a tiny payload such as `{ room, turn }`
  to the player whose turn it is. The game state stays in the mailbox. Push payloads are
  limited to about 4 KB (RFC 8291).
- Clicking the notification opens `#room=<code>`.
- iOS supports Web Push only for web apps added to the Home Screen.

## Acceptance Criteria

- [ ] An opted-in player whose tab is closed gets an "it's your turn" notification.
- [ ] No notifications without explicit opt-in. The VAPID private key is a Worker secret.
