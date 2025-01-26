--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a data type for relationship comparisons between two values.
--|
--| ### Key Concepts
--| * Ord class
--| * Case expressions (pattern matching)
--| * Show instance for a data type
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
