-- http://conal.net/papers/calculating-compilers-categorically
module Data.Stack.Machine where

import Data.Function (($))
import Data.Show (show)
import Data.Stack.Operation (Operation, add, addN, id, pop, push, tuple)
import Data.Stack.Operations (cons, nil)
import Data.Stack.Program (Program(..), evalProgram)
import Data.Tuple (Tuple)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console as Console

-- PureScript doesn't have GADTs; use tagless final encoding instead
-- https://medium.com/@hgiasac/purescript-gadts-alternatives-recap-7960daf4acd8

data StackMachineChoice i o
  = StackMachineChoice
    { input :: i
    , program :: Program i o
    }

ex :: Effect Unit
ex = do
  let
    i = 1
    p = Program
      ( cons (id      :: forall z. Operation (Tuple  Int            z ) (Tuple  Int            z ))
      $ cons (tuple 7 :: forall z. Operation (Tuple  Int            z ) (Tuple (Tuple Int Int) z ))
      $ cons (push    :: forall z. Operation (Tuple (Tuple Int Int) z ) (Tuple  Int (Tuple Int z)))
      $ cons (addN 4  :: forall z. Operation (Tuple  Int            z ) (Tuple  Int            z ))
      $ cons (addN 1  :: forall z. Operation (Tuple  Int            z ) (Tuple  Int            z ))
      $ cons (pop     :: forall z. Operation (Tuple  Int (Tuple Int z)) (Tuple (Tuple Int Int) z ))
      $ cons (add     :: forall z. Operation (Tuple (Tuple Int Int) z ) (Tuple Int             z ))
      $ nil
      )
    b = evalProgram p i
  Console.log $ show b

exCellar :: Effect Unit
exCellar = do
-- TODO: figure out how to define Cellar in terms of Program
--  let
--    p =
--      [ StackChooseCardsFromHand Nothing
--      , StackDuplicate
--      , StackDiscard
--      , StackLength
--      , StackDraw
--      ]
  Console.log $ show "hi"

