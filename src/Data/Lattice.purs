--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defining lattice operations and bounded classes for semilattices.
--|
--| ### Key Concepts
--| * Lattices
--| * Semilattices (specifically join and meet semilattices)
--| * Bounded lattices
module Domination.Data.Lattice where

class JoinSemilattice a where
  join :: a -> a -> a

infixr 4 join as \/

instance joinSemilatticeFunction
  :: JoinSemilattice b => JoinSemilattice (a -> b) where
  join f g a = f a \/ g a

class MeetSemilattice a where
  meet :: a -> a -> a

instance meetSemilatticeFunction
  :: MeetSemilattice b => MeetSemilattice (a -> b) where
  meet f g a = f a /\ g a

infixr 4 meet as /\

class JoinSemilattice a <= BoundedJoinSemilattice a where
  bottom :: a

class MeetSemilattice a <= BoundedMeetSemilattice a where
  top :: a
