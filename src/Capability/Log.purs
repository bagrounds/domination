module Domination.Capability.Log where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

class Monad m <= Log m where
  log :: String -> m Unit
  error :: String -> m Unit

instance logHalogenM :: Log m => Log (HalogenM st act slots msg m) where
  log  = lift <<< log
  error  = lift <<< error

