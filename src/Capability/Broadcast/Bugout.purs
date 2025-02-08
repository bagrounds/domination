--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Defines a broadcaster capability for sending messages over a network.
--|
--| ### Key Concepts
--| * **Broadcasting**: a mechanism for sending messages over a network.
--| * **Monads for Broadcasting**: specialized monads (`AppM`, `HalogenM`) that provide a way to work with broadcasting in different contexts (e.g., applicative effects, Halogen state management).
--| * **FFI Integration**: the module provides an interface to interact with a low-level library (`Bugout`) using Foreign Function Interface (FFI).
module Domination.Capability.Broadcast.Bugout where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either)
import Effect.Aff (Aff, Error, makeAff)
import Effect.Class (liftEffect)
import FFI as FFI

newtype BugoutBroadcaster = BugoutBroadcaster FFI.Bugout

instance showBugoutBroadcaster :: Show BugoutBroadcaster where
  show (BugoutBroadcaster bugout) = FFI.showBugout bugout

broadcastBugoutMessage :: BugoutBroadcaster -> String -> Aff Unit
broadcastBugoutMessage (BugoutBroadcaster bugout) = liftEffect <<< FFI.send bugout

createBugoutBroadcaster :: String -> String -> String -> String -> Aff (Either Error BugoutBroadcaster)
createBugoutBroadcaster roomCode remoteMessageTarget localMessageTarget serverUrl =
  try $ BugoutBroadcaster <$> makeAff
    (FFI.makeBugout roomCode remoteMessageTarget localMessageTarget serverUrl)

getBugoutAddress :: BugoutBroadcaster -> Aff String
getBugoutAddress (BugoutBroadcaster bugout) = liftEffect $ FFI.address bugout
