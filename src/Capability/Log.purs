module Domination.Capability.Log where

import Prelude

import Control.Monad.Trans.Class (lift)
import Domination.AppM (AppM)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Halogen (HalogenM)

class Monad m <= Log m where
  log :: String -> m Unit
  error :: String -> m Unit

instance logHalogenM :: Log m => Log (HalogenM st act slots msg m) where
  log  = lift <<< log
  error  = lift <<< error

instance logAppM :: Log AppM where
  log = liftEffect <<< Console.log
  error = liftEffect <<< Console.error

