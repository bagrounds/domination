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
