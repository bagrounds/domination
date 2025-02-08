--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a broadcaster capability for sending messages over a network.
--|
--| ### Key Concepts
--| * **Broadcasting**: a mechanism for sending messages over a network.
--| * **Monads for Broadcasting**: specialized monads (`AppM`, `HalogenM`) that provide a way to work with broadcasting in different contexts (e.g., applicative effects, Halogen state management).
--| * **FFI Integration**: the module provides an interface to interact with a low-level library (`Bugout`) using Foreign Function Interface (FFI).
module Domination.Capability.Broadcast where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Domination.AppM (AppM)
import Domination.Capability.Broadcast.Bugout (BugoutBroadcaster)
import Domination.Capability.Broadcast.Bugout as Bugout
import Domination.Capability.Broadcast.WebSocket (WebSocketBroadcaster)
import Domination.Capability.Broadcast.WebSocket as WebSocket
import Domination.Capability.Log (class Log, log)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM)

-- Generic broadcaster type that can be implemented by different backends
class Monad m <= Broadcast b m where
  create :: String -> String -> String -> String -> m (Either Error b)
  address :: b -> m String
  broadcast :: b -> String -> m Unit

instance webSocketBroadcasterAppM :: Broadcast WebSocketBroadcaster AppM where
  create roomCode remoteMessageTarget localMessageTarget announce =
    liftAff $ WebSocket.createWebSocketBroadcaster roomCode remoteMessageTarget localMessageTarget announce
  address = liftAff <<< WebSocket.getWebSocketAddress
  broadcast broadcaster = liftAff <<< WebSocket.broadcastWebSocketMessage broadcaster

instance bugoutBroadcasterAppM :: Broadcast BugoutBroadcaster AppM where
  create roomCode remoteMessageTarget localMessageTarget announce =
    liftAff $ Bugout.createBugoutBroadcaster roomCode remoteMessageTarget localMessageTarget announce
  address = liftAff <<< Bugout.getBugoutAddress
  broadcast broadcaster = liftAff <<< Bugout.broadcastBugoutMessage broadcaster

instance broadcastHalogenM :: Broadcast b m => Broadcast b (HalogenM st act slots msg m) where
  create a b c = lift <<< create a b c
  address = lift <<< address
  broadcast :: b -> String -> (HalogenM st act slots msg m) Unit
  broadcast broadcaster = lift <<< broadcast broadcaster

newtype BroadcastM a = BroadcastM (Aff a)

derive newtype instance functorBroadcastM :: Functor BroadcastM
derive newtype instance applyBroadcastM :: Apply BroadcastM
derive newtype instance applicativeBroadcastM :: Applicative BroadcastM
derive newtype instance bindBroadcastM :: Bind BroadcastM
derive newtype instance monadBroadcastM :: Monad BroadcastM
derive newtype instance monadEffectBroadcastM :: MonadEffect BroadcastM
derive newtype instance monadAffBroadcastM :: MonadAff BroadcastM

runBroadcastM :: BroadcastM ~> Aff
runBroadcastM (BroadcastM m) = m

maybeCreateBroadcaster
  :: forall b m
  . Log m
  => Broadcast b m
  => String
  -> String
  -> String
  -> String
  -> m (Maybe b)
maybeCreateBroadcaster roomCode remoteMessageTarget localMessageTarget announce = do
  eBroadcaster <- create roomCode remoteMessageTarget localMessageTarget announce
  case eBroadcaster of
    Left e -> do
      log $ "Error creating broadcaster: " <> show e
      pure Nothing
    Right b -> pure (Just b)
