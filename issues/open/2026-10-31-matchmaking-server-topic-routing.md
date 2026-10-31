# Matchmaking: Optional Topic Routing on the Relay

**Source:** [Matchmaking design](../docs/2026-10-31-matchmaking-design.md) §6.4, §9.1

**Status:** Open (only if traffic or privacy needs it)

## Description

The relay sends every room's traffic to every client. Clients drop messages for other
rooms, so this is correct, but bandwidth grows with total activity, and any modified
client can read any room.

If that becomes a problem, the relay could route by an opaque topic in the connection URL
(`wss://…/?topic=<room>`) without reading message contents. The server would stay a
content-agnostic pub/sub broker. Clients keep filtering by header, so this is only an
optimization (end-to-end argument).

Server routing alone does not give privacy. Privacy would also need hard-to-guess room
codes, or end-to-end encryption with a key in the URL fragment.

## Acceptance Criteria

- [ ] Relay change of a few lines, backward compatible: no topic means the global topic.
- [ ] Clients behave identically with and without the routing relay.
