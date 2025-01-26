--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines various data types, functions, and rules for working with logic statements in a domain-specific language (DSL).
--|
--| ### Key Concepts
--| * **Monads for Error Handling**: Understanding how `MonadError` is used to handle errors in the code.
--| * **Lens**: Knowing how Lens (a type of generic lens) is used to define getters and setters for rule fields.
--| * **Logic Type System**: Understanding the basics of the `Logic` type system, including its syntax, instances, and operations.

module Rule where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array (length)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Setter (over)
import Data.Symbol (SProxy(..))
import Relationship (Relationship, is)

type Predicate a = a -> Boolean

type Rule a =
  { error :: a -> Logic String
  , predicate :: Predicate a
  }

type Ruled =
  { error :: Logic String
  , ok :: Boolean
  }

_error :: forall a. Lens' (Rule a) (a -> Logic String)
_error = prop (SProxy :: SProxy "error")

_predicate :: forall a. Lens' (Rule a) (Predicate a)
_predicate = prop (SProxy :: SProxy "predicate")

check
  :: forall m
  . MonadError String m
  => Ruled
  -> m Unit
check { error, ok } =
  if ok
  then pure unit
  else throwError $ print error

data Logic a
  = False
  | True
  | Claim a
  | Not (Logic a)
  | And (Logic a) (Logic a)
  | Or (Logic a) (Logic a)
  | Implies (Logic a) (Logic a)

derive instance eqLogic :: Eq (Logic String)
derive instance genericLogic :: Generic (Logic a) _
instance showLogic :: Show (Logic String) where
  show a = genericShow a
derive instance ordLogic :: Ord (Logic String)
derive instance functorLogic :: Functor Logic

instance logicHeytingAlgebra :: HeytingAlgebra (Logic a) where
  ff = False
  tt = True
  not a = Not a
  conj a b = And a b
  disj a b = Or a b
  implies a b = Implies a b

print :: Logic String -> String
print = case _ of
  False -> "FALSE"
  True -> "TRUE"
  Claim claim -> claim
  Not l -> "NOT (" <> print l <> ")"
  And l r -> "(" <> print l <> " AND " <> print r <> ")"
  Or l r -> "(" <> print l <> " OR " <> print r <> ")"
  Implies l r -> "IF (" <> print l <> ") then (" <> print r <> ")"

rule :: forall a. Show a => Predicate a -> (String -> String) -> Rule a
rule predicate error = { error: Claim <<< error <<< show, predicate }

infixr 4 rule as !->

enforce :: forall a. Rule a -> a -> Ruled
enforce { error, predicate } x = { error: error x, ok: predicate x }

infixr 4 enforce as !@>

enforceOn :: forall a. a -> Rule a -> Ruled
enforceOn = flip enforce

infixr 4 enforceOn as <@!

simpleRule :: forall a. Show a => Predicate a -> String -> Rule a
simpleRule predicate string = rule predicate (const string)

infixr 4 simpleRule as !>

constantRule :: forall a. Show a => Boolean -> String -> Rule a
constantRule b s = simpleRule (const b) s

infixr 4 constantRule as !!>

appendErrorOn :: forall a. Rule a -> String -> Rule a
appendErrorOn r s = over _error (map $ map (_ <> " " <> s)) r

infixr 4 appendErrorOn as !<>

appendError :: forall a. String -> Rule a -> Rule a
appendError = flip appendErrorOn

infixr 4 appendError as <>!

lengthIs
  :: forall a
  . Show a
  => Relationship
  -> Int
  -> Rule (Array a)
lengthIs r n = length >>> (is r n)
  !> "must have " <> show r <> " " <> show n
