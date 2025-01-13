--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines associativity property for categories with function types.
--|
--| ### Key Concepts
--| * Associative Category class with right and left associativity constraints.
--| * Function instance for the -> type to satisfy these constraints.
module Data.AssociativeCategory where

import Data.Tuple (Tuple(..))

class AssociativeCategory k where
  rassoc
    :: forall a b c
    . k (Tuple (Tuple a b) c) (Tuple a (Tuple b c))
  lassoc
    :: forall a b c
    . k (Tuple a (Tuple b c)) (Tuple (Tuple a b) c)

instance associativeCategoryFunction :: AssociativeCategory (->) where
  rassoc (Tuple (Tuple a b) c) = Tuple a (Tuple b c)
  lassoc (Tuple a (Tuple b c)) = Tuple (Tuple a b) c
