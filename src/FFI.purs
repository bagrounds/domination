module FFI where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Canceler, Error)
import Message (WireMessage)
import Web.Event.Event (Event)

foreign import copyToClipboard :: String -> Effect Unit
foreign import detail :: Event -> String

foreign import data Bugout :: Type

foreign import makeBugout
  :: forall a b
  . String
  -> (a -> b -> Tuple a b) -- Tuple
  -> (Int -> WireMessage) -- ConnectionsWireMessage
  -> (String -> WireMessage) -- SeenWireMessage
  -> (a -> Either Error a) -- Left
  -> (a -> Either Error a) -- Right
  -> (Either Error Bugout -> Effect Unit)
  -> Effect Canceler

foreign import send :: Bugout -> String -> Effect Unit

foreign import address :: Bugout -> Effect String

foreign import genUuid :: Effect String

foreign import arrayBufferAsString :: ArrayBuffer -> String

foreign import stringAsArrayBuffer :: String -> ArrayBuffer

