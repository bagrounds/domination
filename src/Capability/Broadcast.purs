module Domination.Capability.Broadcast where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Domination.AppM (AppM)
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import FFI as FFI
import Halogen (HalogenM)
import Message (WireMessage(..), mkConnectionsWireMessage)

newtype Broadcaster = Broadcaster FFI.Bugout

class Monad m <= Broadcast m where
  create :: String -> m Broadcaster
  address :: Broadcaster -> m String
  broadcast :: Broadcaster -> String -> m Unit

instance broadcastHalogenM :: Broadcast m => Broadcast (HalogenM st act slots msg m) where
  broadcast broadcaster = lift <<< broadcast broadcaster
  create = lift <<< create
  address = lift <<< address

instance broadcastAppM :: Broadcast AppM where
  broadcast broadcaster = liftAff <<< broadcastMessage broadcaster
  create = liftAff <<< createBroadcaster
  address = liftAff <<< getAddress

newtype BroadcastM a = BroadcastM (Aff a)

derive newtype instance functorBroadcastM :: Functor BroadcastM
derive newtype instance applyBroadcastM :: Apply BroadcastM
derive newtype instance applicativeBroadcastM :: Applicative BroadcastM
derive newtype instance bindBroadcastM :: Bind BroadcastM
derive newtype instance monadBroadcastM :: Monad BroadcastM
derive newtype instance monadEffectBroadcastM :: MonadEffect BroadcastM
derive newtype instance monadAffBroadcastM :: MonadAff BroadcastM

instance broadcastBroadcastM :: Broadcast BroadcastM where
  broadcast broadcaster = liftAff <<< broadcastMessage broadcaster
  create = liftAff <<< createBroadcaster
  address = liftAff <<< getAddress

runBroadcastM :: BroadcastM ~> Aff
runBroadcastM (BroadcastM m) = liftAff m

broadcastMessage :: Broadcaster -> String -> Aff Unit
broadcastMessage (Broadcaster bugout) = liftEffect <<< FFI.send bugout

createBroadcaster :: String -> Aff Broadcaster
createBroadcaster roomCode = Broadcaster
  <$> makeAff (FFI.makeBugout roomCode Tuple mkConnectionsWireMessage SeenWireMessage Left Right)

getAddress :: Broadcaster -> Aff String
getAddress (Broadcaster bugout) = liftEffect $ FFI.address bugout

