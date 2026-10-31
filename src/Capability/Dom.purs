--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a Monad class for handling events in a Halogen application.
--|
--| ### Key Concepts
--| * **Monads for Event Handling**: Understanding how monads are used to manage event handling in the Dom module.
--| * **Propagation Control**: Knowing how to use `stopPropagation` to control event propagation in a Halogen application.
--| * **Effect Lift**: Recognizing how effects are lifted from internal effects to external effects using `liftEffect`.
module Domination.Capability.Dom where

import Prelude

import Control.Monad.Trans.Class (lift)
import Domination.AppM (AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML (Window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window

class Monad m <= Dom m where
  stopPropagation :: Event -> m Unit
  window :: m Window
  locationHash :: m String
  setLocationHash :: String -> m Unit
  reload :: m Unit
  pageBaseUrl :: m String

instance domHalogenM
  :: Dom m => Dom (HalogenM st act slots msg m) where
  stopPropagation  = lift <<< stopPropagation
  window = lift window
  locationHash = lift locationHash
  setLocationHash = lift <<< setLocationHash
  reload = lift reload
  pageBaseUrl = lift pageBaseUrl

instance domAppM :: Dom AppM where
  stopPropagation = liftEffect <<< Event.stopPropagation
  window = liftEffect HTML.window
  locationHash = liftEffect $ withLocation Location.hash
  setLocationHash = liftEffect <<< withLocation <<< Location.setHash
  reload = liftEffect $ withLocation Location.reload
  pageBaseUrl = liftEffect $ withLocation \location ->
    append <$> Location.origin location <*> Location.pathname location

newtype DomM a = DomM (Effect a)

derive newtype instance functorDomM :: Functor DomM
derive newtype instance applyDomM :: Apply DomM
derive newtype instance applicativeDomM :: Applicative DomM
derive newtype instance bindDomM :: Bind DomM
derive newtype instance monadDomM :: Monad DomM
derive newtype instance monadEffectDomM :: MonadEffect DomM

instance domDomM :: Dom DomM where
  stopPropagation = liftEffect <<< Event.stopPropagation
  window = liftEffect HTML.window
  locationHash = liftEffect $ withLocation Location.hash
  setLocationHash = liftEffect <<< withLocation <<< Location.setHash
  reload = liftEffect $ withLocation Location.reload
  pageBaseUrl = liftEffect $ withLocation \location ->
    append <$> Location.origin location <*> Location.pathname location

withLocation :: forall a. (Location.Location -> Effect a) -> Effect a
withLocation f = HTML.window >>= Window.location >>= f

runDomM :: DomM ~> Effect
runDomM (DomM m) = liftEffect m
