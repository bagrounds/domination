module Data.Cartesian where

import Data.Tuple (Tuple(..), fst, snd)

class Cartesian k where
  extractLeft :: forall a b. k (Tuple a b) a
  extractRight :: forall a b. k (Tuple a b) b
  duplicate :: forall a. k a (Tuple a a)

instance cartesianFunction :: Cartesian (->) where
  extractLeft = fst
  extractRight = snd
  duplicate a = Tuple a a

