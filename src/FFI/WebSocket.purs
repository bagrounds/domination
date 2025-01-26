module FFI.WebSocket where

import Prelude
import Effect (Effect)

foreign import data WebSocket :: Type

foreign import createImpl :: String -> Effect WebSocket
foreign import onMessageImpl :: WebSocket -> (String -> Effect Unit) -> Effect Unit
foreign import sendImpl :: WebSocket -> String -> Effect Unit
foreign import closeImpl :: WebSocket -> Effect Unit

create :: String -> Effect WebSocket
create = createImpl

onMessage :: WebSocket -> (String -> Effect Unit) -> Effect Unit
onMessage = onMessageImpl

send :: WebSocket -> String -> Effect Unit
send = sendImpl

close :: WebSocket -> Effect Unit
close = closeImpl
