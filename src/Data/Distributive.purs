--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a Distributive class for types involving tuples and either values.
--|
--| ### Key Concepts
--| * Distributive laws
--| * Cartesian product
--| * Cocartesian product
module Data.DistributiveX where

import Data.Cartesian (class Cartesian)
import Data.Cocartesian (class Cocartesian)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

class (Cartesian k, Cocartesian k) <= Distributive k where
  distl
    :: forall a u v
    . k (Tuple a (Either u v)) (Either (Tuple a u) (Tuple a v))
  distr
    :: forall b u v
    . k (Tuple (Either u v) b) (Either (Tuple u b) (Tuple v b))

instance distributiveFunction :: Distributive (->) where
  distl (Tuple a eitherUV) = case eitherUV of
    Left u -> Left (Tuple a u)
    Right v -> Right (Tuple a v)
  distr (Tuple eitherUV b) = case eitherUV of
    Left u -> Left (Tuple u b)
    Right v -> Right (Tuple v b)
