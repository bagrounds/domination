module FFI.Mailbox
  ( MailboxRequest
  , request
  , documentVisible
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Data.Mailbox (HttpResponse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type MailboxRequest =
  { method :: String
  , url :: String
  , ifMatch :: Maybe String
  , ifNoneMatch :: Maybe String
  , body :: Maybe String
  , timeoutMs :: Int
  }

type Header = { name :: String, value :: String }

type RawRequest =
  { method :: String
  , url :: String
  , headers :: Array Header
  , body :: String
  , timeoutMs :: Int
  }

foreign import requestFFI
  :: (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> RawRequest
  -> EffectFnAff HttpResponse

foreign import documentVisible :: Effect Boolean

request :: MailboxRequest -> Aff HttpResponse
request r = fromEffectFnAff $ requestFFI Nothing Just
  { method: r.method
  , url: r.url
  , headers: catMaybes
    [ header "If-Match" <$> r.ifMatch
    , header "If-None-Match" <$> r.ifNoneMatch
    , header "Content-Type" "text/plain; charset=utf-8" <$ r.body
    ]
  , body: fromMaybe "" r.body
  , timeoutMs: r.timeoutMs
  }
  where
    header name value = { name, value }
