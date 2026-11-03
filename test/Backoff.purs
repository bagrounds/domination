module Test.Backoff (backoff_tests) where

import Prelude

import Data.Int (toNumber)
import Domination.Data.Backoff (Backoff, delay, maxDoublings, reconnectBackoff, upperBound)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Test.QuickCheck (Result(..), (<?>))
import Test.QuickCheck.Gen (Gen, choose, chooseInt)
import Test.Room (quick_check)

type EffectTest = { name :: String, test :: Effect Result }

backoff_tests :: Array EffectTest
backoff_tests =
  [ { name: "delay stays within the upper half of the ceiling"
    , test: quick_check 200 do
        attempt <- attemptGen
        sample <- sampleGen
        let
          Milliseconds ceiling = upperBound reconnectBackoff attempt
          Milliseconds wait = delay reconnectBackoff attempt sample
        pure $ (wait >= ceiling / 2.0 && wait <= ceiling)
          <?> "attempt " <> show attempt <> ", sample " <> show sample
            <> ": " <> show wait <> " not in [" <> show (ceiling / 2.0) <> ", " <> show ceiling <> "]"
    }
  , { name: "delay never exceeds the cap and never drops below half the base"
    , test: quick_check 200 do
        backoff <- backoffGen
        attempt <- attemptGen
        sample <- choose (-1.0) 2.0
        let
          Milliseconds wait = delay backoff attempt sample
          Milliseconds base = backoff.base
          Milliseconds cap = backoff.cap
        pure $ (wait <= cap && wait >= min base cap / 2.0)
          <?> show wait <> " escaped [" <> show (min base cap / 2.0) <> ", " <> show cap <> "]"
    }
  , { name: "the ceiling never shrinks as attempts go up"
    , test: quick_check 200 do
        backoff <- backoffGen
        attempt <- attemptGen
        let
          Milliseconds now = upperBound backoff attempt
          Milliseconds next = upperBound backoff (attempt + 1)
        pure $ (next >= now) <?> "attempt " <> show attempt <> ": " <> show next <> " < " <> show now
    }
  , { name: "the ceiling doubles until it reaches the cap"
    , test: quick_check 200 do
        backoff <- backoffGen
        attempt <- chooseInt 0 (maxDoublings - 1)
        let
          Milliseconds now = upperBound backoff attempt
          Milliseconds next = upperBound backoff (attempt + 1)
          Milliseconds cap = backoff.cap
        pure $ (next == min cap (2.0 * now))
          <?> "attempt " <> show attempt <> ": " <> show now <> " -> " <> show next
    }
  , { name: "delay grows with the random sample"
    , test: quick_check 200 do
        attempt <- attemptGen
        a <- sampleGen
        b <- sampleGen
        let
          Milliseconds low = delay reconnectBackoff attempt (min a b)
          Milliseconds high = delay reconnectBackoff attempt (max a b)
        pure $ (low <= high) <?> show low <> " > " <> show high
    }
  , { name: "negative attempts count as the first attempt"
    , test: quick_check 200 do
        attempt <- chooseInt (-1000) 0
        pure $ (upperBound reconnectBackoff attempt == upperBound reconnectBackoff 0)
          <?> "attempt " <> show attempt
    }
  , { name: "the reconnect schedule starts at one second and caps at a minute"
    , test: pure $
        if upperBound reconnectBackoff 0 == Milliseconds 1000.0
          && upperBound reconnectBackoff 6 == Milliseconds 60000.0
          && upperBound reconnectBackoff 1000000 == Milliseconds 60000.0
        then Success
        else Failed "unexpected reconnect schedule"
    }
  ]

attemptGen :: Gen Int
attemptGen = chooseInt 0 1000

sampleGen :: Gen Number
sampleGen = choose 0.0 1.0

backoffGen :: Gen Backoff
backoffGen = do
  base <- toNumber <$> chooseInt 1 10000
  cap <- toNumber <$> chooseInt 1 1000000
  pure { base: Milliseconds base, cap: Milliseconds cap }
