--| When a client looks again for its room's latest game (design §6.4.11).
--|
--| Live play rides the relay, which loses frames whenever a socket is down
--| or silently dead (half-open: phones that sleep and networks that switch
--| leave these behind, and the browser still reports them open). Each
--| `Occasion` below is a moment a client can notice; `reactions` says what
--| it does then. `Main` only performs the reactions.
--|
--| tla/RoomSync.tla checks this policy: with it, every device in a room ends
--| up with the latest game once the trouble is over; without the visible-page
--| poll (game rooms) or the silence reopen (the global room), some do not.
module Domination.Data.Resync
  ( Occasion(..)
  , Clock
  , Reaction(..)
  , reactions
  , pollInterval
  , reopenAfterHidden
  , silenceBackoff
  , allowedSilence
  , longestSilence
  , Marks
  , initialMarks
  , hidden
  , shown
  , pulled
  , heard
  , reopened
  , hiddenFor
  , clockAt
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Domination.Data.Backoff (Backoff, maxDoublings, upperBound)
import Effect.Aff (Milliseconds(..))

-- | `Shown`: the page became visible after being hidden this long.
-- | `Ticked`: the heartbeat timer fired.
data Occasion
  = Started
  | Shown Milliseconds
  | NetworkBack
  | RelayBack
  | PeerJoined
  | RetryDue
  | Ticked Clock

-- | `relayOpen`: the browser says the socket is open (when it knows the
-- | socket closed, it is already reconnecting with backoff).
-- | `sincePull`: since the last mailbox pull. `sinceHeard`: since anything
-- | arrived from the relay, or since the socket was last reopened.
-- | `silentReopens`: reopens since anything last arrived.
type Clock =
  { visible :: Boolean
  , relayOpen :: Boolean
  , sincePull :: Milliseconds
  , sinceHeard :: Milliseconds
  , silentReopens :: Int
  }

-- | `Announce`: send Join and Username. `OfferGame`: where there is no
-- | mailbox, broadcast our versioned game. `ReopenRelay`: drop the socket,
-- | which may be dead without knowing it, and open a new one at once; its
-- | opening is a `RelayBack`.
data Reaction = Announce | OfferGame | PullMailbox | ReopenRelay

derive instance eqReaction :: Eq Reaction
derive instance genericReaction :: Generic Reaction _
instance showReaction :: Show Reaction where
  show = genericShow

reactions :: Occasion -> Array Reaction
reactions = case _ of
  Started -> [ Announce, OfferGame, PullMailbox ]
  Shown away -> [ PullMailbox ] <> (guard (away >= reopenAfterHidden) $> ReopenRelay)
  NetworkBack -> [ PullMailbox, ReopenRelay ]
  RelayBack -> [ Announce, OfferGame, PullMailbox ]
  PeerJoined -> [ OfferGame ]
  RetryDue -> [ PullMailbox ]
  Ticked clock ->
    (guard (clock.visible && clock.sincePull >= pollInterval) $> PullMailbox)
      <> (guard (clock.visible && clock.relayOpen && silentTooLong clock) $> ReopenRelay)

silentTooLong :: Clock -> Boolean
silentTooLong { sinceHeard, silentReopens } = sinceHeard >= allowedSilence silentReopens

-- | A visible page asks the mailbox this often, so a move whose frame was
-- | lost still arrives. About 120 requests an hour per visible page.
pollInterval :: Milliseconds
pollInterval = Milliseconds 30000.0

-- | Sockets that slept this long are likely dead; a fresh one costs little.
reopenAfterHidden :: Milliseconds
reopenAfterHidden = Milliseconds 30000.0

-- | Peers send a heartbeat every 5 s, so hearing nothing for 15 s means we
-- | are alone or our socket is dead. Each reopen that still hears nothing
-- | doubles the wait, up to 2 minutes, so a lone page reopens rarely.
silenceBackoff :: Backoff
silenceBackoff = { base: Milliseconds 15000.0, cap: Milliseconds 120000.0 }

allowedSilence :: Int -> Milliseconds
allowedSilence = upperBound silenceBackoff

longestSilence :: Milliseconds
longestSilence = allowedSilence maxDoublings

-- | When things last happened, on the wall clock (which, unlike the page's
-- | monotonic clock, keeps counting while a phone sleeps).
type Marks =
  { hiddenAt :: Maybe Milliseconds
  , pulledAt :: Milliseconds
  , heardAt :: Milliseconds
  , silentReopens :: Int
  }

initialMarks :: Milliseconds -> Marks
initialMarks at = { hiddenAt: Nothing, pulledAt: at, heardAt: at, silentReopens: 0 }

hidden :: Milliseconds -> Marks -> Marks
hidden at = _ { hiddenAt = Just at }

shown :: Marks -> Marks
shown = _ { hiddenAt = Nothing }

pulled :: Milliseconds -> Marks -> Marks
pulled at = _ { pulledAt = at }

heard :: Milliseconds -> Marks -> Marks
heard at = _ { heardAt = at, silentReopens = 0 }

reopened :: Milliseconds -> Marks -> Marks
reopened at marks = marks { heardAt = at, silentReopens = marks.silentReopens + 1 }

-- | How long the page was hidden, as it becomes visible; never negative,
-- | even if the wall clock was set back.
hiddenFor :: Milliseconds -> Marks -> Milliseconds
hiddenFor at = maybe (Milliseconds 0.0) (since at) <<< _.hiddenAt

clockAt :: Milliseconds -> { visible :: Boolean, relayOpen :: Boolean } -> Marks -> Clock
clockAt at { visible, relayOpen } marks =
  { visible
  , relayOpen
  , sincePull: since at marks.pulledAt
  , sinceHeard: since at marks.heardAt
  , silentReopens: marks.silentReopens
  }

since :: Milliseconds -> Milliseconds -> Milliseconds
since (Milliseconds at) (Milliseconds before) = Milliseconds $ max 0.0 (at - before)
