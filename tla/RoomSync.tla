----------------------------- MODULE RoomSync -----------------------------
(***************************************************************************)
(* How the devices in a room keep its game in sync (design §6.4.11).       *)
(*                                                                         *)
(* Moves travel two ways. The room mailbox (when HasMailbox) is a          *)
(* compare-and-set register that numbers games with revisions; devices     *)
(* pull it. The relay forwards frames only to live sockets. A socket can   *)
(* be live, closed ("down": the browser noticed and reconnects by itself)  *)
(* or half-open (dead, but the browser still thinks it is open: phones     *)
(* that sleep and networks that switch leave these behind). Without a      *)
(* mailbox, games carry Lamport versions <<time, device>> and the newest   *)
(* wins (Domination.Data.LogicalClock).                                    *)
(*                                                                         *)
(* Each action is an Occasion of Domination.Data.Resync, and its effect is *)
(* the Reactions that module lists for it. Policy = "v0.40" is what v0.40.0 *)
(* did; "v0.41" adds a mailbox poll while the page is visible, a reopen of *)
(* the socket after a long nap, and a reopen when the relay has been       *)
(* silent too long.                                                        *)
(*                                                                         *)
(* The mailbox's own conflict handling (stamps, tentative moves, uncertain *)
(* writes) is abstracted to an atomic compare-and-set; test/Mailbox.purs   *)
(* checks it with the real code. Here the question is whether the          *)
(* occasions are enough: once the chaos stops, does every device end up    *)
(* with the latest game, without anyone reloading?                         *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
    N,            \* devices
    MaxMoves,     \* moves made in the whole run
    MaxNaps,      \* times any device sleeps
    MaxBreaks,    \* sockets that die while awake
    MaxOutages,   \* mailbox outages
    HasMailbox,   \* a game room with a mailbox, or the global room
    Policy        \* "v0.40" or "v0.41"

ASSUME Policy \in {"v0.40", "v0.41"} /\ HasMailbox \in BOOLEAN

Devices == 1..N
Links == {"live", "halfopen", "down"}
NoVersion == <<0, 0>>
Versions == ({NoVersion} \cup ((1..MaxMoves) \X Devices))

\* Lamport order: time, then device.
Before(a, b) == a[1] < b[1] \/ (a[1] = b[1] /\ a[2] < b[2])
Max(a, b) == IF a > b THEN a ELSE b
Fixed == Policy = "v0.41"

VARIABLES
    awake,     \* the page is visible
    link,      \* the socket's real state
    rev,       \* the mailbox revision whose game a device shows
    ver,       \* the version of the game a device shows (no mailbox)
    mrev,      \* the mailbox's revision
    up,        \* the mailbox answers
    retry,     \* a pull failed and a retry is scheduled
    net,       \* frames the relay is carrying
    moves, naps, breaks, outages

vars == <<awake, link, rev, ver, mrev, up, retry, net, moves, naps, breaks, outages>>
chaos == <<moves, naps, breaks, outages>>

Frame(to, kind, val) == [to |-> to, kind |-> kind, val |-> val]

\* What device d's broadcast puts on the relay: nothing unless its socket is
\* really live, and only for sockets the relay holds (not closed ones).
Broadcast(d, kind, val, l) ==
    IF l[d] = "live"
    THEN {Frame(e, kind, val) : e \in {x \in Devices \ {d} : l[x] # "down"}}
    ELSE {}

\* OfferGame: only without a mailbox.
Offer(d, l, v) == IF HasMailbox THEN {} ELSE Broadcast(d, "game", v[d], l)

\* Announce, OfferGame, PullMailbox: the reactions to Started and RelayBack.
Announced(d, l, v) == Broadcast(d, "join", 0, l) \cup Offer(d, l, v)

Pulled(d) == IF HasMailbox /\ up THEN Max(rev[d], mrev) ELSE rev[d]
PullFails(d) == HasMailbox /\ ~up

PullMailbox(d) ==
    /\ rev' = [rev EXCEPT ![d] = Pulled(d)]
    /\ retry' = [retry EXCEPT ![d] = retry[d] \/ PullFails(d)]

\* ReopenRelay, then RelayBack's reactions.
Reopen(d) ==
    LET l == [link EXCEPT ![d] = "live"]
    IN /\ link' = l
       /\ net' = net \cup Announced(d, l, ver)
       /\ PullMailbox(d)

Init ==
    /\ awake = [d \in Devices |-> TRUE]
    /\ link = [d \in Devices |-> "live"]
    /\ rev = [d \in Devices |-> 0]
    /\ ver = [d \in Devices |-> NoVersion]
    /\ mrev = 0
    /\ up = TRUE
    /\ retry = [d \in Devices |-> FALSE]
    /\ net = {}
    /\ moves = 0 /\ naps = 0 /\ breaks = 0 /\ outages = 0

----------------------------------------------------------------------------
\* The players and the world: bounded, and never required to happen.

Move(d) ==
    /\ awake[d] /\ moves < MaxMoves
    /\ moves' = moves + 1
    /\ IF HasMailbox
       THEN /\ up
            /\ IF rev[d] = mrev
               THEN /\ mrev' = mrev + 1
                    /\ rev' = [rev EXCEPT ![d] = mrev + 1]
                    /\ net' = net \cup Broadcast(d, "game", mrev + 1, link)
               \* 412: the move lost to a newer game, which the device takes.
               ELSE /\ rev' = [rev EXCEPT ![d] = mrev]
                    /\ UNCHANGED <<mrev, net>>
            /\ UNCHANGED ver
       ELSE LET v == [ver EXCEPT ![d] = <<ver[d][1] + 1, d>>]
            IN /\ ver' = v
               /\ net' = net \cup Broadcast(d, "game", v[d], link)
               /\ UNCHANGED <<rev, mrev>>
    /\ UNCHANGED <<awake, link, up, retry, naps, breaks, outages>>

Sleep(d) ==
    /\ awake[d] /\ naps < MaxNaps
    /\ naps' = naps + 1
    /\ awake' = [awake EXCEPT ![d] = FALSE]
    /\ \E l \in Links :
         link' = [link EXCEPT ![d] = IF link[d] = "live" THEN l ELSE link[d]]
    /\ UNCHANGED <<rev, ver, mrev, up, retry, net, moves, breaks, outages>>

Break(d) ==
    /\ link[d] = "live" /\ breaks < MaxBreaks
    /\ breaks' = breaks + 1
    /\ \E l \in {"halfopen", "down"} : link' = [link EXCEPT ![d] = l]
    /\ UNCHANGED <<awake, rev, ver, mrev, up, retry, net, moves, naps, outages>>

Outage ==
    /\ up /\ outages < MaxOutages
    /\ up' = FALSE /\ outages' = outages + 1
    /\ UNCHANGED <<awake, link, rev, ver, mrev, retry, net, moves, naps, breaks>>

----------------------------------------------------------------------------
\* What the devices and the network do by themselves.

\* Shown: the page is visible again, after a short or a long nap.
Wake(d) ==
    /\ ~awake[d]
    /\ awake' = [awake EXCEPT ![d] = TRUE]
    /\ \E long \in BOOLEAN :
         IF Fixed /\ long
         THEN Reopen(d)
         ELSE /\ PullMailbox(d)
              /\ UNCHANGED <<link, net>>
    /\ UNCHANGED <<ver, mrev, up>> /\ UNCHANGED chaos

\* The socket noticed its close and its reconnect timer fired: RelayBack.
Reconnect(d) ==
    /\ link[d] = "down"
    /\ Reopen(d)
    /\ UNCHANGED <<awake, ver, mrev, up>> /\ UNCHANGED chaos

Restore ==
    /\ ~up /\ up' = TRUE
    /\ UNCHANGED <<awake, link, rev, ver, mrev, retry, net>> /\ UNCHANGED chaos

RetryDue(d) ==
    /\ retry[d] /\ up
    /\ rev' = [rev EXCEPT ![d] = Max(rev[d], mrev)]
    /\ retry' = [retry EXCEPT ![d] = FALSE]
    /\ UNCHANGED <<awake, link, ver, mrev, up, net>> /\ UNCHANGED chaos

\* Ticked, visible, mailbox room: pull (only the pulls that change
\* something are steps).
Poll(d) ==
    /\ Fixed /\ HasMailbox /\ awake[d] /\ up /\ rev[d] < mrev
    /\ rev' = [rev EXCEPT ![d] = mrev]
    /\ UNCHANGED <<awake, link, ver, mrev, up, retry, net>> /\ UNCHANGED chaos

\* Ticked while visible, and nothing heard for too long (peers send
\* heartbeats, so silence means we are alone or our socket is dead). A
\* half-open socket hears nothing, so the silence always comes. Reopening a
\* live socket changes nothing, so those steps are left out.
Silence(d) ==
    /\ Fixed /\ awake[d] /\ link[d] = "halfopen"
    /\ Reopen(d)
    /\ UNCHANGED <<awake, ver, mrev, up>> /\ UNCHANGED chaos

\* The relay hands a frame to a socket; a socket that is not live loses it.
Receive(f) ==
    /\ f \in net
    /\ LET rest == net \ {f}
           d == f.to
       IN IF link[d] # "live"
          THEN /\ net' = rest
               /\ UNCHANGED <<rev, ver>>
          ELSE IF f.kind = "join"
          THEN /\ net' = rest \cup Offer(d, link, ver)          \* PeerJoined
               /\ UNCHANGED <<rev, ver>>
          ELSE IF HasMailbox
          THEN /\ rev' = [rev EXCEPT ![d] = Max(rev[d], f.val)] \* Committed r
               /\ net' = rest
               /\ UNCHANGED ver
          ELSE IF Before(ver[d], f.val)                        \* TakeTheirs
          THEN /\ ver' = [ver EXCEPT ![d] = f.val]
               /\ net' = rest
               /\ UNCHANGED rev
          ELSE IF Before(f.val, ver[d])                        \* SendOurs
          THEN /\ net' = rest \cup Offer(d, link, ver)
               /\ UNCHANGED <<rev, ver>>
          ELSE /\ net' = rest                                  \* AlreadyHave
               /\ UNCHANGED <<rev, ver>>
    /\ UNCHANGED <<awake, link, mrev, up, retry>> /\ UNCHANGED chaos

Next ==
    \/ \E d \in Devices :
         Move(d) \/ Sleep(d) \/ Break(d) \/ Wake(d) \/ Reconnect(d)
           \/ RetryDue(d) \/ Poll(d) \/ Silence(d)
    \/ Outage \/ Restore
    \/ \E f \in net : Receive(f)

\* People come back, sockets reconnect, timers fire, the mailbox recovers
\* and the relay delivers. Nothing obliges anyone to move or sleep.
Fairness ==
    /\ \A d \in Devices :
         /\ WF_vars(Wake(d)) /\ WF_vars(Reconnect(d)) /\ WF_vars(RetryDue(d))
         /\ WF_vars(Poll(d)) /\ WF_vars(Silence(d))
    /\ WF_vars(Restore)
    /\ WF_vars(\E f \in net : Receive(f))

Spec == Init /\ [][Next]_vars /\ Fairness

----------------------------------------------------------------------------
TypeOK ==
    /\ awake \in [Devices -> BOOLEAN]
    /\ link \in [Devices -> Links]
    /\ rev \in [Devices -> 0..MaxMoves]
    /\ ver \in [Devices -> Versions]
    /\ mrev \in 0..MaxMoves
    /\ up \in BOOLEAN
    /\ retry \in [Devices -> BOOLEAN]

\* Safety: nobody shows a game the mailbox never committed...
NeverAheadOfMailbox == \A d \in Devices : rev[d] <= mrev

\* ...and nobody ever goes back to an older game.
NoRewind ==
    [][\A d \in Devices : rev'[d] >= rev[d] /\ ~Before(ver'[d], ver[d])]_vars

\* Liveness: once the chaos is over, everyone ends up with the latest game
\* and stays there.
Converged ==
    IF HasMailbox
    THEN \A d \in Devices : rev[d] = mrev
    ELSE \A d, e \in Devices : ver[d] = ver[e]

EventuallyConverged == <>[]Converged
=============================================================================
