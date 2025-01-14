--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Provides a Monad class for generating unique IDs, with various instance implementations for HalogenM, AppM, and Effect.
--|
--| ### Key Concepts
--| * Monad for generating UUIDs
--| * Lift functions for converting between different monads
--| * Effectful monad for generating UUIDs with side effects
module Domination.Capability.GenUuid where

import Prelude

import Control.Monad.Trans.Class (lift)
import Domination.AppM (AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FFI as FFI
import Halogen (HalogenM)

class Monad m <= GenUuid m where
  genUuid :: m String

instance genUuidHalogenM :: GenUuid m => GenUuid (HalogenM st act slots msg m) where
  genUuid  = lift genUuid

instance genUuidAppM :: GenUuid AppM where
  genUuid = liftEffect FFI.genUuid

newtype GenUuidM a = GenUuidM (Effect a)

derive newtype instance functorGenUuidM :: Functor GenUuidM
derive newtype instance applyGenUuidM :: Apply GenUuidM
derive newtype instance applicativeGenUuidM :: Applicative GenUuidM
derive newtype instance bindGenUuidM :: Bind GenUuidM
derive newtype instance monadGenUuidM :: Monad GenUuidM
derive newtype instance monadEffectGenUuidM :: MonadEffect GenUuidM

instance genUuidGenUuidM :: GenUuid GenUuidM where
  genUuid = liftEffect FFI.genUuid

runGenUuidM :: GenUuidM ~> Effect
runGenUuidM (GenUuidM m) = liftEffect m
