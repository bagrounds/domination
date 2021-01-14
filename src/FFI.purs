module FFI where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Canceler, Error)
import Web.Event.Event (Event)

foreign import copyToClipboard :: String -> Effect Unit
foreign import detail :: Event -> String

foreign import data Bugout :: Type

foreign import makeBugout
  :: forall a
  . String
  -> (a -> Either Error a)
  -> (a -> Either Error a)
  -> (Either Error Bugout -> Effect Unit)
  -> Effect Canceler

foreign import send :: Bugout -> String -> Effect Unit

foreign import address :: Bugout -> Effect String

foreign import genUuid :: Effect String

