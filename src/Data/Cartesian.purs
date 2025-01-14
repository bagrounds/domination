--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a Cartesian class for functions, enabling left, right extraction and duplicate functionality.
--|
--| ### Key Concepts
--| * Type class for Cartesian transformations
--| * Tuple data structure
--| * Function composition (instance of the `Cartesian` type class)
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
