module Comm where
import Prelude
import Halogen.Query.EventSource
import Data.Either
import Effect.Aff
import Effect.Aff.Class
import Effect
import Effect.Class

foreign import data PeerConnection :: Type
foreign import mkPeerConnection :: Unit -> PeerConnection
foreign import getLocalDescription :: forall a. PeerConnection -> (a -> Either Error a) -> ((Either Error a -> Effect Unit) -> Effect Canceler)
foreign import data DataChannel :: Type
foreign import mkDataChannel :: forall a. String -> PeerConnection -> (a -> Either Error a) -> ((Either Error a -> Effect Unit) -> Effect Canceler)
foreign import joinDataChannel :: forall a. String -> PeerConnection -> (a -> Either Error a) -> ((Either Error a -> Effect Unit) -> Effect Canceler)
foreign import setRemoteDescription :: String -> PeerConnection -> Effect Unit

instance showDataChannel :: Show DataChannel where show _ = "DataChannel"
instance showPeerConnection :: Show PeerConnection where show _ = "PeerConnection"

foreign import log :: forall a. a -> Effect Unit

foreign import create :: forall a. (a -> Either Error a) -> ((Either Error a -> Effect Unit) -> Effect Canceler)
foreign import gotAnswer :: String -> Effect Unit
foreign import say :: String -> Effect Unit
foreign import join :: forall a. String -> (a -> Either Error a) -> ((Either Error a -> Effect Unit) -> Effect Canceler)
foreign import windowLocalDescription :: Effect String
