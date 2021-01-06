module Message where

import Prelude

import Data.Argonaut (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Domination.Data.GameState (GameState)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML (text) as HH
import Halogen.HTML.Elements (div, span) as HH
import Halogen.HTML.Properties (class_) as HH

type Envelope = { id :: String, message :: Message }

wrapMessage :: String -> Message -> Envelope
wrapMessage id message = { id, message }

data Message
  = ChatMessage { username :: String, message :: String }
  | UsernameMessage { username :: String, id :: String }
  | GameStateMessage GameState
  | SeenMessage String
  | ConnectionsMessage Int

derive instance genericMessage :: Generic Message _
instance encodeJsonMessage :: EncodeJson Message where
  encodeJson = genericEncodeJson
instance decodeJsonMessage :: DecodeJson Message where
  decodeJson = genericDecodeJson

renderHtml :: forall w i. Message -> HTML w i
renderHtml (ChatMessage { username, message }) =
  HH.div
    [ HH.class_ $ ClassName "chat-message" ]
    [ HH.span [ HH.class_ $ ClassName "username" ] [ HH.text username ]
    , HH.text ": "
    , HH.span [ HH.class_ $ ClassName "message" ] [ HH.text message ]
    ]
renderHtml (GameStateMessage _) =
  HH.div
    [ HH.class_ $ ClassName "game-state-message" ]
    [ HH.div [ HH.class_ $ ClassName "game-state" ] [ HH.text "(Game Data Received)" ]
    ]
renderHtml (SeenMessage address) =
  HH.div
    [ HH.class_ $ ClassName "seen-message" ]
    [ HH.text "(New Connection: "
    , HH.span [ HH.class_ $ ClassName "address" ] [ HH.text address ]
    , HH.text ")"
    ]
renderHtml (ConnectionsMessage count) =
  HH.div
    [ HH.class_ $ ClassName "connections-message" ]
    [ HH.text "("
    , HH.span [ HH.class_ $ ClassName "address" ] [ HH.text $ show count ]
    , HH.text " Connections)"
    ]
renderHtml (UsernameMessage { username, id }) =
  HH.div
    [ HH.class_ $ ClassName "username-message" ]
    [ HH.text "("
    , HH.span [ HH.class_ $ ClassName "username" ] [ HH.text $ show username ]
    , HH.text " has ID: "
    , HH.span [ HH.class_ $ ClassName "username" ] [ HH.text $ show id ]
    , HH.text ")"
    ]
