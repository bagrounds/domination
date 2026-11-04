# TLA+ model of room sync

[`RoomSync.tla`](RoomSync.tla) models how the devices in a room keep its game in sync:
the room mailbox (a compare-and-set register), the relay (which only reaches live
sockets), sockets that are live, closed or half-open (dead while the browser still
thinks they are open), mailbox outages, and devices that sleep and wake. Its actions are
the occasions of [`Domination.Data.Resync`](../src/Data/Resync.purs), and their effects
are that module's reactions. The design is in
[§6.4.11 of the async play design](../docs/2026-11-03-async-play-design.md).

It checks, for every behaviour of two devices with a few moves, naps, broken sockets
and an outage:

| Property | Kind | Meaning |
|---|---|---|
| `TypeOK` | invariant | Variables stay in range |
| `NeverAheadOfMailbox` | invariant | Nobody shows a game the mailbox never committed |
| `NoRewind` | action property | Nobody ever goes back to an older game |
| `EventuallyConverged` | liveness | Once the trouble is over, every device ends up with the latest game and stays there |

Each `RoomSync_<room>_<policy>.cfg` says what TLC must find on its first line:

| Config | Expected | Why |
|---|---|---|
| `mailbox_v0.40` | liveness violation | A device that missed a frame (its socket, or the sender's, died silently) never pulls again while it stays visible |
| `relay_v0.40` | liveness violation | In the global room nothing replaces a half-open socket |
| `mailbox_v0.41` | no error | Visible pages poll the mailbox |
| `relay_v0.41` | no error | A socket that hears nothing for too long is reopened |

Removing the poll from v0.41 breaks the mailbox room, and removing the silence reopen
breaks the global room. Reopening after a long nap only makes catching up faster.

## Running it

```sh
tla/check
```

It needs Java 11 or later and downloads a pinned `tla2tools.jar` (v1.7.4, checked by
SHA-256) into `tla/.tools/` on first use. Set `TLA2TOOLS` to use another jar. A full
run takes about three minutes; it is not part of `npm test`.

To run one config by hand:

```sh
cd tla
java -cp .tools/tla2tools-v1.7.4.jar tlc2.TLC -workers auto -deadlock \
  -config RoomSync_relay_v0.40.cfg RoomSync.tla
```

`-deadlock` is needed because the chaos is bounded: once it is used up, behaviours end
in stuttering, which is not a deadlock here.
