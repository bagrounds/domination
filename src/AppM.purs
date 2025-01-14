--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A module for creating monads with effects, specifically the AppM type.
--|
--| ### Key Concepts
--| * Reader Monad
--| * Effect System
--| * Monad Instances
module Domination.AppM where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Domination.Env (Env)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
