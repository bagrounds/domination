--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Provides functions for working with tuples in PureScript, allowing extraction and duplication of tuple elements.
--|
--| ### Key Concepts
--| * Type class `Cartesian` for functions of type `(->)`
--| * Laws for the laws of function types: `extractLeft`, `extractRight`, and `duplicate`
--| * Example instance for `Function -> Cartesian`
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
