module Relationship where

import Data.Eq ((/=), (==))
import Data.Ord (class Ord, (<), (<=), (>))
import Prelude (class Show)

data Relationship
  = EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE

instance showRelationship :: Show Relationship where
  show = case _ of
    EQ -> "exactly"
    NEQ -> "not"
    LT -> "less than"
    LTE -> "at most"
    GT -> "more than"
    GTE -> "at least"

is :: forall a. Ord a => Relationship -> a -> a -> Boolean
is r n = case r of
  EQ -> (_ == n)
  NEQ -> (_ /= n)
  LT -> (_ < n)
  LTE -> (_ <= n)
  GT -> (_ > n)
  GTE -> (_ > n)

