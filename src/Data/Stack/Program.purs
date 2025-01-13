--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A data type for representing programs, with various instance definitions providing category and monoidal product operations.
--|
--| ### Key Concepts
--| * A category where the identity element is of type `forall z. Operations (Tuple a z) (Tuple b z)`
--| * A monoidal product with special instances for swapping and extracting elements
--| * A primitive that can be lifted to a program operation, along with a function to create a stack function from a program
module Data.Stack.Program where

import Control.Category (class Category, (<<<))
import Control.Semigroupoid (class Semigroupoid)
import Data.BraidedCategory (class BraidedCategory, swap)
import Data.Cartesian (class Cartesian, duplicate, extractLeft, extractRight)
import Data.MonoidalProduct (class MonoidalProduct, first, second)
import Data.Stack.Function (StackFunction(..), evalStackFunction)
import Data.Stack.Operation (pop, primitive, push)
import Data.Stack.Operations (Operations(..), cons, evalOperations, nil)
import Data.Stack.Primitive (Primitive)
import Data.Tuple (Tuple)

newtype Program a b =
  Program (forall z. Operations (Tuple a z) (Tuple b z))

instance semigroupoidProgram :: Semigroupoid Program where
  compose (Program f) (Program g) = Program (f <<< g)

instance categoryProgram :: Category Program where
  identity = Program nil

instance braidedCategoryProgram
  :: BraidedCategory Program where
  swap = Program (Operations (first swap))

instance monoidalProductProgram
  :: MonoidalProduct Program where
  first (Program operations) =
    Program (pop `cons` nil <<< push `cons` operations)
  second g = swap <<< first g <<< swap
  cross f g = first f <<< second g

instance cartesianProgram :: Cartesian Program where
  extractLeft = primitiveProgram extractLeft
  extractRight = primitiveProgram extractRight
  duplicate = primitiveProgram duplicate

programFunction :: forall a b. Program a b -> StackFunction a b
programFunction (Program operations) =
  StackFunction (evalOperations operations)

evalProgram :: forall a b. Program a b -> a -> b
evalProgram = evalStackFunction <<< programFunction

primitiveProgram :: forall a b. Primitive a b -> Program a b
primitiveProgram p = Program (primitive p `cons` nil)
