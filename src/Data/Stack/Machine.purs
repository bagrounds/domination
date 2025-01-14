--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a stack machine with basic operations like push, pop, add, subtract, multiply, and divide, using a program that evaluates to a specific input result.
--|
--| ### Key Concepts
--| * Stack Machine: a simple abstract machine for evaluating mathematical expressions.
--| * Program: a sequence of stack operations that can be executed on input values.
--| * Evaluation: the process of running a program on an input value, resulting in an output value.
module Data.Stack.Machine where


import Data.Function (($))
import Data.Show (show)
import Data.Stack.Operation (Operation, add, addN, divN, id, mulN, pop, push, subN, tuple)
import Data.Stack.Operations (cons, nil)
import Data.Stack.Program (Program(..), evalProgram)
import Data.Tuple (Tuple)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console as Console

data StackMachineChoice i o
  = StackMachineChoice
    { input :: i
    , program :: Program i o
    }

exampleStackMachineComputation :: Int -> Int
exampleStackMachineComputation input = let
  -- A stack program operates on a value (X) and a stack.
  -- We can apply functions to X,
  --        push values on the stack,
  --    and pop values off the stack.
  -- The program itself is a list of stack operatons.
  program = Program
    -- (1 nil)         start with the input (X) and an empty stack (nil)
    ( cons (id      :: forall z. Operation (Tuple  Int            z ) (Tuple  Int            z ))
    -- (1 nil)         apply the identity function to X
    $ cons (tuple 7 :: forall z. Operation (Tuple  Int            z ) (Tuple (Tuple Int Int) z ))
    -- ((7 1) nil)     pair 7 with X
    $ cons (push    :: forall z. Operation (Tuple (Tuple Int Int) z ) (Tuple  Int (Tuple Int z)))
    -- (7 (1 nil))     push (snd X) onto the stack
    $ cons (addN 4  :: forall z. Operation (Tuple  Int            z ) (Tuple  Int            z ))
    -- (11 (1 nil))    add 4 to X
    $ cons (subN 1  :: forall z. Operation (Tuple  Int            z ) (Tuple  Int            z ))
    -- (10 (1 nil))    subtract 1 from X
    $ cons (pop     :: forall z. Operation (Tuple  Int (Tuple Int z)) (Tuple (Tuple Int Int) z ))
    -- ((10 1) nil)    pop the top element off the stack and pair X with it
    $ cons (add     :: forall z. Operation (Tuple (Tuple Int Int) z ) (Tuple Int             z ))
    -- (11 nil)        add (fst X) and (snd X)
    $ cons (mulN 3  :: forall z. Operation (Tuple  Int            z ) (Tuple Int             z ))
    -- (33 nil)        multiply X by 3
    $ cons (divN 11 :: forall z. Operation (Tuple  Int            z ) (Tuple Int             z ))
    -- (3 nil)         divide X by 11
    $ nil
    )
  in
    evalProgram program input

exCellar :: Effect Unit
exCellar = do
  Console.log $ show "hi"
