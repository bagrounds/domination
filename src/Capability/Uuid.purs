module Domination.Capability.GenUuid where

import Prelude

import Control.Monad.Trans.Class (lift)
import Domination.AppM (AppM)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import FFI as FFI
import Halogen (HalogenM)

class Monad m <= GenUuid m where
  genUuid :: m String

instance genUuidHalogenM :: GenUuid m => GenUuid (HalogenM st act slots msg m) where
  genUuid  = lift genUuid

instance genUuidAppM :: GenUuid AppM where
  genUuid = liftEffect FFI.genUuid

newtype GenUuidM a = GenUuidM (Aff a)

derive newtype instance functorGenUuidM :: Functor GenUuidM
derive newtype instance applyGenUuidM :: Apply GenUuidM
derive newtype instance applicativeGenUuidM :: Applicative GenUuidM
derive newtype instance bindGenUuidM :: Bind GenUuidM
derive newtype instance monadGenUuidM :: Monad GenUuidM
derive newtype instance monadEffectGenUuidM :: MonadEffect GenUuidM
derive newtype instance monadAffGenUuidM :: MonadAff GenUuidM

instance genUuidGenUuidM :: GenUuid GenUuidM where
  genUuid = liftEffect FFI.genUuid

runGenUuidM :: GenUuidM ~> Aff
runGenUuidM (GenUuidM m) = liftAff m

