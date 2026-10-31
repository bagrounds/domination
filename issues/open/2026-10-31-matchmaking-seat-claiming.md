# Matchmaking: Seat Claiming

**Source:** [Matchmaking design](../docs/2026-10-31-matchmaking-design.md) §6.3

**Status:** Open

## Description

Players in a room still pick their seat with the *Player #* control and agree on it in
chat, so two players can end up in the same seat. Share each member's chosen seat in
presence data, so the control can show which seats are taken and disable them.

## Approach

- Add the seat to presence data without changing the `WireMessage` layout, for example
  through a `dom/1` header extension or a body that old clients decode harmlessly. Do not
  append `WireMessage` constructors.
- Resolve conflicts with a single-owner rule (Steam): the lowest live member id keeps the seat.

## Acceptance Criteria

- [ ] Taken seats are visible and disabled in Settings → Game while in a room.
- [ ] Conflict resolution is a pure, property-tested function.
