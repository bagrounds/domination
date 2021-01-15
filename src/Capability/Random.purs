module Domination.Capability.Random where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Except (except)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array (drop, length, take, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Domination.AppM (AppM)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Ex
import Effect.Random (randomInt)
import Halogen.Query.HalogenM (HalogenM)
import Unsafe.Coerce (unsafeCoerce)
import Util (dropIndex)

class Monad m <= Random m where
  shuffle :: forall a. Eq a => Array a -> m (Array a)
  randomElement :: forall a. Array a -> m (Maybe a)

instance randomHalogenM :: Random m => Random (HalogenM st act slots msg m) where
  shuffle = lift <<< shuffle
  randomElement = lift <<< randomElement

newtype RandomM a = RandomM (Aff a)

derive newtype instance functorRandomM :: Functor RandomM
derive newtype instance applyRandomM :: Apply RandomM
derive newtype instance applicativeRandomM :: Applicative RandomM
derive newtype instance bindRandomM :: Bind RandomM
derive newtype instance monadRandomM :: Monad RandomM
derive newtype instance monadEffectRandomM :: MonadEffect RandomM
derive newtype instance monadAffRandomM :: MonadAff RandomM

instance exceptTStringRandomM :: Random m => Random (ExceptT String m) where
  shuffle xs = pure xs >>= lift <<< shuffle
  randomElement xs = pure xs >>= lift <<< randomElement

instance randomRandomM :: Random RandomM where
  shuffle = liftEffect <<< randomShuffle
  randomElement = liftEffect <<< pickRandomElement

runRandomM :: RandomM ~> Aff
runRandomM (RandomM m) = liftAff m

instance randomAppM :: Random AppM where
  shuffle = liftEffect <<< randomShuffle
  randomElement = liftEffect <<< pickRandomElement

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

