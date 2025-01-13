--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A module for working with sets in PureScript.
--|
--| ### Key Concepts
--| * Set data structure
--| * Union and intersection operations on Sets
--| * Join and Meet semilattices on Sets
module Domination.Data.Table2 where

import Prelude

import Data.List.Lazy (List, nil)
import Data.List.Lazy as List

newtype Set a = Set (List a)

empty :: forall a. Set a
empty = Set nil

union :: forall a. Ord a => Set a -> Set a -> Set a
union (Set s1) (Set s2) = Set $ List.union s1 s2

intersect :: forall a. Ord a => Set a -> Set a -> Set a
intersect (Set s1) (Set s2) = Set $ List.intersect s1 s2

class JoinSemilattice a where
  join :: a -> a -> a

infixr 4 join as \/

instance joinSemilatticeSet :: Ord a => JoinSemilattice (Set a) where
  join = union

class MeetSemilattice a where
  meet :: a -> a -> a

infixr 4 meet as /\

instance meetSemilatticeSet :: Ord a => MeetSemilattice (Set a) where
  meet = intersect

class JoinSemilattice a <= BoundedJoinSemilattice a where
  bottom :: a

instance boundedJoinSemilatticeSet
  :: Ord a => BoundedJoinSemilattice (Set a) where
  bottom = empty
