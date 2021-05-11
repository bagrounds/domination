module Domination.UI.Chat where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (fromMaybe)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Message (RemoteMessage(..))
import Message as Message
import Web.UIEvent.KeyboardEvent as KE

type State r =
  { usernames :: HashMap String String
  , messages :: Array RemoteMessage
  , message :: String
  | r
  }

type RenderInput a r =
  { state :: State r
  , onInput :: String -> a
  , nothing :: a
  , sendEvent :: a
  }

render :: forall s a r. RenderInput a r -> HTML s a
render { sendEvent, onInput, state, nothing } =
  let { usernames, messages, message } = state
  in HH.div
  [ HP.class_ $ ClassName "chat" ]
  [ HH.div
    [ HP.class_ $ ClassName "chat-history" ]
    $
    ( Message.renderHtml
      <<< case _ of
        ChatMessage { username, message } ->
          ChatMessage
            { username: fromMaybe username
              $ HashMap.lookup username usernames
            , message
            }
        y -> y
      <$> messages
    )
  , HH.div
    [ HP.class_ $ ClassName "chat-form"]
    [ HH.button
      [ HE.onClick $ const $ sendEvent
      , HP.class_ $ ClassName "send-chat"
      ]
      [ HH.text "Send" ]
    , HH.input
      [ HP.type_ HP.InputText
      , HP.class_ $ ClassName "chat-input"
      , HH.attr (H.AttrName "aria-label") "Chat"
      , HP.value message
      , HP.required true
      , HE.onValueInput $ onInput
      , HE.onKeyDown \e ->
        if (KE.key e) == "Enter"
        then sendEvent
        else nothing
      ]
    ]
  ]

