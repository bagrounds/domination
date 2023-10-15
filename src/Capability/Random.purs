module Domination.Capability.Random where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (drop, fromFoldable, length, take, uncons, (!!))
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Domination.AppM (AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomBool, randomInt)
import Halogen.Query.HalogenM (HalogenM)
import Util (dropIndex)

class Monad m <= Random m where
  shuffle :: forall a. Eq a => Array a -> m (Array a)
  randomElement :: forall a. Array a -> m (Maybe a)
  randomIntBetween :: Int -> Int -> m Int
  randomBoolean :: m Boolean

unfoldingShuffle
  :: forall f g a m
  . Random m
  => Foldable f
  => Unfoldable g
  => Eq a
  => f a
  -> m (g a)
unfoldingShuffle = map (unfoldr step) <<< shuffle <<< fromFoldable
  where
    step = map toTuple <<< uncons
    toTuple { head, tail } = Tuple head tail

instance randomHalogenM :: Random m => Random (HalogenM st act slots msg m) where
  shuffle = lift <<< shuffle
  randomElement = lift <<< randomElement
  randomIntBetween a = lift <<< randomIntBetween a
  randomBoolean = lift randomBoolean

newtype RandomM a = RandomM (Effect a)

derive newtype instance functorRandomM :: Functor RandomM
derive newtype instance applyRandomM :: Apply RandomM
derive newtype instance applicativeRandomM :: Applicative RandomM
derive newtype instance bindRandomM :: Bind RandomM
derive newtype instance monadRandomM :: Monad RandomM
derive newtype instance monadEffectRandomM :: MonadEffect RandomM

instance exceptTStringRandomM
  :: Random m => Random (ExceptT String m) where
  shuffle xs = pure xs >>= lift <<< shuffle
  randomElement xs = pure xs >>= lift <<< randomElement
  randomIntBetween a = lift <<< randomIntBetween a
  randomBoolean = lift randomBoolean

instance randomRandomM :: Random RandomM where
  shuffle = liftEffect <<< randomShuffle
  randomElement = liftEffect <<< pickRandomElement
  randomIntBetween a = liftEffect <<< randomInt a
  randomBoolean = liftEffect randomBool

runRandomM :: RandomM ~> Effect
runRandomM (RandomM m) = liftEffect m

instance randomAppM :: Random AppM where
  shuffle = liftEffect <<< randomShuffle
  randomElement = liftEffect <<< pickRandomElement
  randomIntBetween a = liftEffect <<< randomInt a
  randomBoolean = liftEffect randomBool

randomShuffle
  :: forall a m
  . MonadEffect m
  => Eq a
  => Array a -> m (Array a)
randomShuffle array = fst <$> shuffle' (Tuple [] array)
  where
    shuffle' :: Tuple (Array a) (Array a) -> m (Tuple (Array a) (Array a))
    shuffle' (Tuple shuffled []) = pure $ (Tuple shuffled [])
    shuffle' (Tuple shuffled unshuffled) = do
      i <- liftEffect $ randomInt 0 (length unshuffled - 1)
      let randomElement' = take 1 $ drop i unshuffled
      let unshuffledRemainder = dropIndex i unshuffled
      shuffle' (Tuple (randomElement' <> shuffled) unshuffledRemainder)

pickRandomElement :: forall a m . MonadEffect m => Array a -> m (Maybe a)
pickRandomElement xs = do
  i <- liftEffect $ randomInt 0 (length xs - 1)
  pure $ xs !! i

