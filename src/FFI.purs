module FFI where
import Prelude
import Data.Either (Either)
import Effect.Aff (Canceler, Error)
import Effect (Effect)
import Web.Event.Event (Event)

foreign import detail :: Event -> String

foreign import create :: forall a. Int -> (a -> Either Error a) -> (Either Error a -> Effect Unit) -> Effect Canceler

foreign import gotAnswer :: Int -> String -> Effect Unit
foreign import say :: String -> Effect Unit
foreign import join :: forall a. String -> (a -> Either Error a) -> ((Either Error a -> Effect Unit) -> Effect Canceler)
foreign import windowLocalDescription :: Effect String

foreign import copyToClipboard :: String -> Effect Unit
