--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Provides a class for defining braided categories, specifically for function types.
--|
--| ### Key Concepts
--| * Braided Category
--| * Swapping data types
--| * Typeclass instance
module Data.BraidedCategory where

import Data.Tuple (Tuple(..))

class BraidedCategory k where
  swap :: forall a b. k (Tuple a b) (Tuple b a)

instance braidedCategoryFunction :: BraidedCategory (->) where
  swap (Tuple a b) = Tuple b a
