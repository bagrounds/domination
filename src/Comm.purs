module Comm where
import Prelude
import Halogen.Query.EventSource
import Data.Either
import Effect.Aff
import Effect.Aff.Class
import Effect
import Effect.Class
import Web.Event.Event (Event)

foreign import log :: forall a. a -> Effect Unit
foreign import detail :: Event -> String

foreign import create :: forall a. (a -> Either Error a) -> (Either Error a -> Effect Unit) -> Effect Canceler

foreign import gotAnswer :: String -> Effect Unit
foreign import say :: String -> Effect Unit
foreign import join :: forall a. String -> (a -> Either Error a) -> ((Either Error a -> Effect Unit) -> Effect Canceler)
foreign import windowLocalDescription :: Effect String

type StoreItemContext =
  { right :: String -> Either String Unit
  , left :: String -> Either String Unit
  , unit :: Unit
  }
foreign import storeItem :: forall m. MonadEffect m => StoreItemContext -> String -> String -> m (Either String Unit)

--storeItem :: forall m. MonadEffect m => String -> String -> m (Either String Unit)
--storeItem = storeItemJs
--  { right
--  , left
--  , unit: unit
--  }
--  where
--    right :: String -> Either String Unit
--    right s = Right s
--    left :: String -> Either String Unit
--    left s = Left s
