module Data.MonoidalProduct where

import Control.Category ((<<<))
import Data.BraidedCategory (swap)
import Data.Tuple (Tuple(..))

class MonoidalProduct k where
  cross :: forall a b c d. k a c -> k b d -> k (Tuple a b) (Tuple c d)
  first :: forall a b c. k a c -> k (Tuple a b) (Tuple c b)
  second :: forall a b d. k b d -> k (Tuple a b) (Tuple a d)

instance monoidalProductFunction :: MonoidalProduct (->) where
  cross f g = first f <<< second g
  first f (Tuple a b) = Tuple (f a) b
  second g = swap <<< first g <<< swap

