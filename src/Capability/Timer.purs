--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Creates periodic timers using Halogen subscriptions.
--|
--| ### Key Concepts
--| * Effect management for timers
--| * Halogen subscription handling
--| * Timer state configuration

module Domination.Capability.Timer where

import Prelude

import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Int (toNumber)
import Domination.AppM (AppM)
import Effect.Aff (Milliseconds(..), delay, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM)
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS

class Monad m <= Timer m where
  createTimer :: forall a. { interval :: Int } -> a -> m (Emitter a)

instance timerHalogenM 
  :: Timer m 
  => Timer (HalogenM st act slots msg m) where
  createTimer opts = lift <<< createTimer opts

newtype TimerM a = TimerM (HalogenM {} {} () {} AppM a)

derive newtype instance functorTimerM :: Functor TimerM
derive newtype instance applyTimerM :: Apply TimerM
derive newtype instance applicativeTimerM :: Applicative TimerM
derive newtype instance bindTimerM :: Bind TimerM
derive newtype instance monadTimerM :: Monad TimerM
derive newtype instance monadEffectTimerM :: MonadEffect TimerM
derive newtype instance monadAffTimerM :: MonadAff TimerM

instance timerTimerM :: Timer TimerM where
  createTimer opts = liftAff <<< createAffTimer opts

instance timerAppM :: Timer AppM where
  createTimer opts = liftAff <<< createAffTimer opts

createAffTimer 
  :: forall m a
  . MonadAff m
  => { interval :: Int }
  -> a 
  -> m (Emitter a)
createAffTimer { interval } val = do
  { emitter, listener } <- liftEffect HS.create
  _ <- liftAff $ forkAff $ forever do
    delay $ Milliseconds (toNumber interval)
    liftEffect $ HS.notify listener val
  pure emitter

