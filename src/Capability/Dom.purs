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
import Data.Int (toNumber)
import Domination.AppM (AppM)
import Domination.UI.Placement (Rect, Size)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM)
import Web.DOM.Element (Element, getBoundingClientRect)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML (Window)
import Web.HTML.Window (innerHeight, innerWidth)

class Monad m <= Dom m where
  stopPropagation :: Event -> m Unit
  window :: m Window
  boundingClientRect :: Element -> m Rect
  viewportSize :: m Size

instance domHalogenM
  :: Dom m => Dom (HalogenM st act slots msg m) where
  stopPropagation  = lift <<< stopPropagation
  window = lift window
  boundingClientRect = lift <<< boundingClientRect
  viewportSize = lift viewportSize

instance domAppM :: Dom AppM where
  stopPropagation = liftEffect <<< Event.stopPropagation
  window = liftEffect HTML.window
  boundingClientRect = liftEffect <<< measureElement
  viewportSize = liftEffect measureViewport

measureElement :: Element -> Effect Rect
measureElement = map toRect <<< getBoundingClientRect
  where
  toRect { left, top, width, height } = { left, top, width, height }

measureViewport :: Effect Size
measureViewport = do
  w <- HTML.window
  { width: _, height: _ }
    <$> (toNumber <$> innerWidth w)
    <*> (toNumber <$> innerHeight w)

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
  boundingClientRect = liftEffect <<< measureElement
  viewportSize = liftEffect measureViewport

runDomM :: DomM ~> Effect
runDomM (DomM m) = liftEffect m
