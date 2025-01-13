--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Provides logging functionality for different monadic contexts.
--|
--| ### Key Concepts
--| * MonadEffect typeclass for logging effects
--| * Functor, Applicative, and Monad instances for `LogM`
--|  
--| Note: `AppM` is not included in the list as it's not a part of the standard library. It might be a custom type or module specific to the Domination library.

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
