--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Creates a user interface component for inputting usernames.
--|
--| ### Key Concepts
--| * State Management
--| * Render Input Functionality
--| * Event Handling for Input Changes
module Domination.UI.UsernameInput where

import Prelude

import Domination.UI.Css as Css
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State r =
  { username :: String
  | r
  }

type RenderInput r1 r a =
  { state :: State r
  , onInput :: String -> a
  | r1
  }

render :: forall s r a r1. RenderInput r1 r a -> HTML s a
render { onInput, state: { username } } = HH.div
  [ HP.class_ Css.usernameInput ]
  [ HH.label_ [ HH.text "Username: " ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.value username
    , HP.placeholder "Username"
    , HP.required true
    , HE.onValueInput $ onInput
    ]
  ]
