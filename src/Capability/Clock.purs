module Domination.Capability.Clock where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Trans.Class (lift)
import Domination.AppM (AppM)
import Domination.Capability.Random (RandomM)
import Effect.Class (liftEffect)
import FFI as FFI
import Halogen (HalogenM)

class Monad m <= Clock m where
  now :: m Int

instance clockHalogenM
  :: Clock m => Clock (HalogenM st act slots msg m) where
  now = lift now

instance clockRandomM :: Clock RandomM where
  now = liftEffect FFI.now

instance clockAppM :: Clock AppM where
  now = liftEffect FFI.now

instance clockExceptTString :: Clock m => Clock (ExceptT String m) where
  now = lift now
