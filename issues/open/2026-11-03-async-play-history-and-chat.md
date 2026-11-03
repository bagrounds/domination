# Async Play: History and Chat While Away

**Source:** [Async play design](../../docs/2026-11-03-async-play-design.md) §6.5

**Status:** Open (after the room mailbox)

## Description

The mailbox gives a returning player the latest state, but the game log only shows the
last play, and chat messages sent while they were away are lost.

## Approach

- The room's Durable Object also keeps the last 200 envelopes, each with a server sequence
  number.
- `GET /v1/rooms/{code}/history?after=<seq>` returns what the client has not seen yet,
  like Matrix `since` and XMPP XEP-0313.
- Chat messages in game rooms are written through as well.
- The client replays missed game envelopes in order into the log, and shows missed chat
  messages.

## Acceptance Criteria

- [ ] A returning player sees every play and chat message since their last visit, up to
      the history limit.
- [ ] The history stays bounded per room and expires with the room.
