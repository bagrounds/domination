--| ## Reconnect backoff
--|
--| How long a client waits before it tries to reach the relay again. The
--| ceiling doubles with every failed attempt until it reaches a cap
--| (exponential backoff), and the delay is drawn from the upper half of the
--| ceiling (equal jitter). Clients that lost the relay at the same moment, for
--| example during a Render restart, come back spread out rather than all at
--| once, and none of them retries sooner than half the ceiling.
--|
--| See docs/2026-11-03-async-play-design.md §6.3.
module Domination.Data.Backoff
  ( Backoff
  , reconnectBackoff
  , maxDoublings
  , upperBound
  , delay
  ) where

import Prelude

import Data.Int (pow, toNumber)
import Effect.Aff (Milliseconds(..))

type Backoff = { base :: Milliseconds, cap :: Milliseconds }

reconnectBackoff :: Backoff
reconnectBackoff = { base: Milliseconds 1000.0, cap: Milliseconds 60000.0 }

-- | Doublings beyond this count cannot matter for any sensible cap, and 2^30
-- | still fits in an `Int`.
maxDoublings :: Int
maxDoublings = 30

-- | The longest a client waits before attempt number `attempt` (counted from 0).
upperBound :: Backoff -> Int -> Milliseconds
upperBound { base: Milliseconds base, cap: Milliseconds cap } attempt =
  Milliseconds $ min cap $ base * toNumber (pow 2 doublings)
  where
    doublings = clamp 0 maxDoublings attempt

-- | The delay before attempt number `attempt`, given a uniform sample from
-- | [0, 1] such as `Math.random()`. Samples outside that range are clamped.
delay :: Backoff -> Int -> Number -> Milliseconds
delay backoff attempt sample = Milliseconds $ half + half * clamp 0.0 1.0 sample
  where
    Milliseconds ceiling = upperBound backoff attempt
    half = ceiling / 2.0
