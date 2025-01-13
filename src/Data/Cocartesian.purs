--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines an "inject" function for left and right injection, as well as "jamming", into the Either type.
--|
--| ### Key Concepts
--| * A class `Cocartesian k` defines a type constructor `k` with three methods: `injectLeft`, `injectRight`, and `jam`.
--| * The class provides instances for the function type `(->)`.
--| * `injectLeft` and `injectRight` are used to inject left or right values into an `Either` value, while `jam` is used to "jam" a single value into an `Either` value.
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
