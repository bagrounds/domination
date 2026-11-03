module Domination.Capability.Mailbox
  ( class Mailbox
  , requestMailbox
  , isDocumentVisible
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Domination.AppM (AppM)
import Domination.Data.Mailbox (HttpResponse)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import FFI.Mailbox (MailboxRequest)
import FFI.Mailbox as FFI
import Halogen (HalogenM)

class Monad m <= Mailbox m where
  requestMailbox :: MailboxRequest -> m HttpResponse
  isDocumentVisible :: m Boolean

instance mailboxHalogenM :: Mailbox m => Mailbox (HalogenM st act slots msg m) where
  requestMailbox = lift <<< requestMailbox
  isDocumentVisible = lift isDocumentVisible

instance mailboxAppM :: Mailbox AppM where
  requestMailbox = liftAff <<< FFI.request
  isDocumentVisible = liftEffect FFI.documentVisible
