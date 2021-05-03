module Data.BraidedCategory where

import Data.Tuple (Tuple(..))

class BraidedCategory k where
  swap :: forall a b. k (Tuple a b) (Tuple b a)

instance braidedCategoryFunction :: BraidedCategory (->) where
  swap (Tuple a b) = Tuple b a

