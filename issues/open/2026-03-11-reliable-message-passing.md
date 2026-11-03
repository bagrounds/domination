# Reliable message passing

**Source:** README TODO list

**Status:** Open

## Description

Improve the reliability of peer-to-peer message passing. The current WebRTC-based communication through Bugout can be unreliable, with messages occasionally being dropped or connections being lost.

## Potential Approaches

- Implement message acknowledgment and retry logic
- Add message ordering guarantees
- Implement state synchronization protocol
- Consider fallback communication channels
- Add connection health monitoring and automatic reconnection

## See Also

- [Async play design](../docs/2026-11-03-async-play-design.md). It found that sends after a
  reconnect go to the closed socket ([reconnect sends](../closed/2026-11-03-async-play-reconnect-sends.md)),
  and it proposes a room mailbox with conditional writes so that missed game messages are
  recovered ([room mailbox](./2026-11-03-async-play-mailbox.md)).
