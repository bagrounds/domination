--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Provides a custom Monad for logging messages with different modes (Halogen, RandomM, AppM).
--|
--| ### Key Concepts
--| * **Monad**: A concept in functional programming that allows for the abstraction of effects.
--| * **Logging**: The capability to log messages with a given monad, used for tracking or debugging purposes.
--| * **Effect**: A type-level concept representing a computational effect, such as input/output operations.

module Domination.Capability.Log where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Trans.Class (lift)
import Domination.AppM (AppM)
import Domination.Capability.Random (RandomM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen (HalogenM)

class Monad m <= Log m where
  log :: String -> m Unit
  error :: String -> m Unit

instance logHalogenM
  :: Log m => Log (HalogenM st act slots msg m) where
  log  = lift <<< log
  error  = lift <<< error

instance logRandomM :: Log RandomM where
  log = liftEffect <<< Console.log
  error = liftEffect <<< Console.error

instance logAppM :: Log AppM where
  log = liftEffect <<< Console.log
  error = liftEffect <<< Console.error

newtype LogM a = LogM (Effect a)

derive newtype instance functorLogM :: Functor LogM
derive newtype instance applyLogM :: Apply LogM
derive newtype instance applicativeLogM :: Applicative LogM
derive newtype instance bindLogM :: Bind LogM
derive newtype instance monadLogM :: Monad LogM
derive newtype instance monadEffectLogM :: MonadEffect LogM

instance logExceptTString :: Log m => Log (ExceptT String m) where
  log = lift <<< log
  error = lift <<< error

instance logLogM :: Log LogM where
  log = liftEffect <<< Console.log
  error = liftEffect <<< Console.error

runLogM :: LogM ~> Effect
runLogM (LogM m) = liftEffect m
