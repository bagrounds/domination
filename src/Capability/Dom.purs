module Domination.Capability.Dom where

import Prelude

import Control.Monad.Trans.Class (lift)
import Domination.AppM (AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM)
import Web.Event.Event (Event)
import Web.Event.Event as Event

class Monad m <= Dom m where
  stopPropagation :: Event -> m Unit

instance domHalogenM :: Dom m => Dom (HalogenM st act slots msg m) where
  stopPropagation  = lift <<< stopPropagation

instance domAppM :: Dom AppM where
  stopPropagation = liftEffect <<< Event.stopPropagation

newtype DomM a = DomM (Effect a)

derive newtype instance functorDomM :: Functor DomM
derive newtype instance applyDomM :: Apply DomM
derive newtype instance applicativeDomM :: Applicative DomM
derive newtype instance bindDomM :: Bind DomM
derive newtype instance monadDomM :: Monad DomM
derive newtype instance monadEffectDomM :: MonadEffect DomM

instance logDomM :: Dom DomM where
  stopPropagation = liftEffect <<< Event.stopPropagation

runDomM :: DomM ~> Effect
runDomM (DomM m) = liftEffect m

