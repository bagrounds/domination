module Util where

import Prelude

import Data.Array
import Data.Maybe
import Data.Tuple
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)

dropIndex :: forall a. Int -> Array a -> Array a
dropIndex i xs = take i xs <> drop (i + 1) xs

withIndices :: forall a. Array a -> Array (Tuple Int a)
withIndices xs = zip (indices xs) xs

dropIndices :: forall a. Array Int -> Array a -> Maybe (Array a)
dropIndices indices xs =
  if length indices > length xs
  || length (nub indices) /= length indices
  || (_ < 0) `any` indices
  || (_ >= length xs) `any` indices
  then Nothing
  else Just $ snd <$>
  (fst >>> flip notElem indices) `filter` withIndices xs

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
