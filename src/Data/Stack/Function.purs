--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A purescript implementation of a Stack Function category, providing various instances for monoidal product, cartesian product, semigroupoid, braided categories, associative category, and others.
--|
--| ### Key Concepts
--| * **Monoidal Product**: The concept of a monoidal product is crucial in understanding this module.
--| * **Braided Categories**: Understanding braided categories and the related category laws is important for grasping this module's instances.
--| * **Semigroupoids and Categories**: Comprehending semigroupoids and categories, as well as their instances, is necessary to fully understand the abstract concepts presented in this module.
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
