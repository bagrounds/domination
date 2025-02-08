module Domination.UI.ServerUrlInput where

import Prelude

import Domination.UI.Css as Css
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State r =
  { serverUrl :: String
  | r
  }

type RenderInput r1 r a =
  { state :: State r
  , onInput :: String -> a
  | r1
  }

render :: forall s r a r1. RenderInput r1 r a -> HTML s a
render { onInput, state: { serverUrl } } = HH.div
  [ HP.class_ Css.serverUrlInput ]
  [ HH.label_ [ HH.text "Server URL: " ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.value serverUrl
    , HP.placeholder "e.g. wss://purescript-wip.onrender.com"
    , HP.required true
    , HE.onValueInput $ onInput
    ]
  ]
