module Data.Stack.Operations where

import Control.Category (identity, (<<<))
import Control.Semigroupoid (class Semigroupoid)
import Data.Stack.Operation (Operation(..), ReifiedOperation)
import Data.Tuple (Tuple)

class StackOperations ops op | ops -> op where
  nil :: forall a. ops a a
  cons :: forall a b c. op a b -> ops b c -> ops a c

newtype Operations a b = Operations (a -> b)

evalOperations :: forall a b. Operations a b -> (a -> b)
evalOperations (Operations f) = f

instance stackOperationsOperations
  :: StackOperations Operations Operation where
  nil = Operations identity
  cons (Operation op) (Operations ops) = Operations (ops <<< op)

instance semigroupoidOperations :: Semigroupoid Operations where
  compose a b = Operations (evalOperations a <<< evalOperations b)

data ReifiedOperations a b
  = Nil
  | Cons (forall m. Tuple (ReifiedOperation a m) (ReifiedOperations m b))

--instance stackOperationsReifiedOperations
--  :: StackOperations ReifiedOperations ReifiedOperation where
--    nil = Nil
--    cons op ops1 = case ops1 of
--      Nil -> Cons (Tuple op Nil)

