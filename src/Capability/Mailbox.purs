module Domination.Capability.Mailbox
  ( class Mailbox
  , requestMailbox
  , isDocumentVisible
  , wallClock
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Domination.AppM (AppM)
import Domination.Data.Mailbox (HttpResponse)
import Effect.Aff (Milliseconds(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import FFI.Mailbox (MailboxRequest)
import FFI.Mailbox as FFI
import Halogen (HalogenM)

class Monad m <= Mailbox m where
  requestMailbox :: MailboxRequest -> m HttpResponse
  isDocumentVisible :: m Boolean
  wallClock :: m Milliseconds

instance mailboxHalogenM :: Mailbox m => Mailbox (HalogenM st act slots msg m) where
  requestMailbox = lift <<< requestMailbox
  isDocumentVisible = lift isDocumentVisible
  wallClock = lift wallClock

instance mailboxAppM :: Mailbox AppM where
  requestMailbox = liftAff <<< FFI.request
  isDocumentVisible = liftEffect FFI.documentVisible
  wallClock = liftEffect $ Milliseconds <$> FFI.wallClock
