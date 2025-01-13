--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A higher-order function data type for manipulating functions.
--|
--| ### Key Concepts
--| * Semigroupoid: A binary operation on objects that is associative.
--| * Category: A set of objects with a morphism (function) between them, satisfying composition.
--| * Monoidal Product: An object with two projection functors and a bifunctor.
module Data.Stack.Function where

import Control.Category (class Category, identity, (<<<))
import Control.Semigroupoid (class Semigroupoid)
import Data.AssociativeCategory (class AssociativeCategory, lassoc, rassoc)
import Data.BraidedCategory (class BraidedCategory, swap)
import Data.Cartesian (class Cartesian, duplicate, extractLeft, extractRight)
import Data.MonoidalProduct (class MonoidalProduct, first, second)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)

newtype StackFunction a b =
  StackFunction (forall z. Tuple a z -> Tuple b z)

stackFunction :: forall a b. (a -> b) -> StackFunction a b
stackFunction = StackFunction <<< first

evalStackFunction :: forall a b. StackFunction a b -> a -> b
evalStackFunction (StackFunction f) a = b
  where
    Tuple b unit = f (Tuple a unit)

instance semigroupoidStackFunction :: Semigroupoid StackFunction where
  compose (StackFunction g) (StackFunction f) =
    StackFunction (g <<< f)

instance categoryStackFunction :: Category StackFunction where
  identity = StackFunction identity

instance associativeCategoryStackFunction
  :: AssociativeCategory StackFunction where
  rassoc = stackFunction rassoc
  lassoc = stackFunction lassoc

instance braidedCategoryStackFunction
  :: BraidedCategory StackFunction where
  swap = stackFunction swap

instance monoidalProductStackFunction
  :: MonoidalProduct StackFunction where
  first (StackFunction f) = StackFunction (lassoc <<< f <<< rassoc)
  second g = swap <<< first g <<< swap
  cross f g = first f <<< second g

instance cartesianStackFunction :: Cartesian StackFunction where
  extractLeft = stackFunction extractLeft
  extractRight = stackFunction extractRight
  duplicate = stackFunction duplicate
