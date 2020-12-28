module Util
  ( shuffle
  , dropIndex
  , indices
  ) where

import Prelude

import Data.Array (take, drop, length, mapWithIndex)
import Data.Tuple (Tuple(..), fst)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)

dropIndex :: forall a. Int -> Array a -> Array a
dropIndex i xs = take i xs <> drop (i + 1) xs

shuffle :: forall a m . MonadEffect m => Eq a => Array a -> m (Array a)
shuffle array = fst <$> shuffle' (Tuple [] array)
  where
    shuffle' :: Tuple (Array a) (Array a) -> m (Tuple (Array a) (Array a))
    shuffle' (Tuple shuffled []) = pure $ (Tuple shuffled [])
    shuffle' (Tuple shuffled unshuffled) = do
      i <- liftEffect $ randomInt 0 (length unshuffled - 1)
      let randomElement = take 1 $ drop i unshuffled
      let unshuffledRemainder = dropIndex i unshuffled
      shuffle' (Tuple (randomElement <> shuffled) unshuffledRemainder)

indices :: forall a. Array a -> Array Int
indices xs = fst <$> mapWithIndex Tuple xs
