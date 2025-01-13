--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A PureScript module for handling event propagation in a DOM-based application.
--|
--| ### Key Concepts
--| * **Monad of Effects**: Understand the `Dom` class and its instances as a monad for effects.
--| * **Halogen Monad**: Recognize how Halogen's `HalogenM` is related to the `Dom` monad.
--| * **Event Handling**: Know how event propagation is handled in the `Dom` monad.
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

instance domHalogenM
  :: Dom m => Dom (HalogenM st act slots msg m) where
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

instance domDomM :: Dom DomM where
  stopPropagation = liftEffect <<< Event.stopPropagation

runDomM :: DomM ~> Effect
runDomM (DomM m) = liftEffect m
