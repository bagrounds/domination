--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a MonoidalSum class for composing functions over Either values.
--|
--| ### Key Concepts
--| * Monoidal Sum as a Binary Operation
--| * Either Type as a Monoidal Context
--| * Identity for the Monoidal Sum Operation
module Data.MonoidalSum where

import Data.Either (Either(..))

class MonoidalSum k where
  sum :: forall a b c d. k a c -> k b d -> k (Either a b) (Either c d)
  left :: forall a b c. k a c -> k (Either a b) (Either c b)
  right :: forall a b d. k b d -> k (Either a b) (Either a d)

instance monoidalSumFunction :: MonoidalSum (->) where
  sum f g = case _ of
    Left a -> Left (f a)
    Right b -> Right (g b)
  left f = case _ of
    Left a -> Left (f a)
    Right b -> Right b
  right g = case _ of
    Left a -> Left a
    Right b -> Right (g b)
