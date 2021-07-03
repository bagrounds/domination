module Data.Cocartesian where

import Data.Either (Either(..))

class Cocartesian k where
  injectLeft :: forall a b. k a (Either a b)
  injectRight :: forall a b. k b (Either a b)
  jam :: forall a. k (Either a a) a

instance cocartesianFunction :: Cocartesian (->) where
  injectLeft = Left
  injectRight = Right
  jam = case _ of
    Left a -> a
    Right a -> a

