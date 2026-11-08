--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A PureScript module for building a simple chat UI with input field, send button, and message history display.
--|
--| ### Key Concepts
--| * State Management: The state of a chat application is represented by a type `State r` which includes a map of usernames, an array of messages, and a message string.
--| * Rendering: The `render` function takes a `RenderInput a r` as input and returns an HTML representation of the chat application using Halogen.
--| * Event Handling: The module handles keyboard events (specifically Enter key press) to send a new message or clear the input field.

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
import Domination.Data.Chat (Chat, ChatEntry(..), entries)
import Domination.UI.RenderText (renderText)
import Web.UIEvent.KeyboardEvent as KE

type State r =
  { usernames :: HashMap String String
  , messages :: Chat
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
    $ renderEntry usernames <$> entries messages
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

renderEntry :: forall s a. HashMap String String -> ChatEntry -> HTML s a
renderEntry usernames = case _ of
  Said { sender, text } ->
    renderSaid (fromMaybe sender $ HashMap.lookup sender usernames) text
  Notice { text } -> renderSaid noticeSender text
  Played { playerIndex, play } -> HH.div
    [ HP.class_ $ ClassName "play-made-message" ]
    [ HH.text $ "Player " <> show (playerIndex + 1) <> ": "
    , HH.span [ HP.class_ $ ClassName "play-made" ] [ renderText play ]
    ]

renderSaid :: forall s a. String -> String -> HTML s a
renderSaid username text = HH.div
  [ HP.class_ $ ClassName "chat-message" ]
  [ HH.span [ HP.class_ $ ClassName "username" ] [ HH.text username ]
  , HH.text ": "
  , HH.span [ HP.class_ $ ClassName "message" ] [ HH.text text ]
  ]

noticeSender :: String
noticeSender = "📬"
