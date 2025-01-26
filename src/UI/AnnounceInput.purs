--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Provides a UI component for inputting an announcement message.
--|
--| ### Key Concepts
--| * **State and Render Input**: Understand the definitions of `State` and `RenderInput` types.
--| * **Rendering HTML Components**: Recognize how to render an HTML component using the `render` function.
--| * **Event Handling**: Understand how event handling is implemented through the use of `HE.onValueInput` and `onInput` function.
module Domination.UI.AnnounceInput where

import Prelude

import Domination.UI.Css as Css
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State r =
  { announce :: String
  | r
  }

type RenderInput r1 r a =
  { state :: State r
  , onInput :: String -> a
  | r1
  }

render :: forall s r a r1. RenderInput r1 r a -> HTML s a
render { onInput, state: { announce } } = HH.div
  [ HP.class_ Css.announceInput ]
  [ HH.label_ [ HH.text "Announce: " ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.value announce
    , HP.placeholder "e.g. wss://p2p-tracker-24is.onrender.com, wss://tracker.btorrent.xyz"
    , HP.required true
    , HE.onValueInput $ onInput
    ]
  ]
