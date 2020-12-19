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

foreign import create :: forall a b. (a -> Either Error a) -> (Either Error a -> Effect Unit) -> Effect Canceler

foreign import gotAnswer :: String -> Effect Unit
foreign import say :: String -> Effect Unit
foreign import join :: forall a. String -> (a -> Either Error a) -> ((Either Error a -> Effect Unit) -> Effect Canceler)
foreign import windowLocalDescription :: Effect String
