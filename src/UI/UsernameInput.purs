module Domination.UI.UsernameInput where

import Prelude

import Data.Maybe (Maybe(..))
import Domination.UI.Css as Css
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State r =
  { username :: String
  | r
  }

type RenderInput r a =
  { state :: State r
  , onInput :: String -> a
  }

render :: forall s r a. RenderInput r a -> HTML s a
render { onInput, state: { username } } = HH.div
  [ HP.class_ Css.usernameInput ]
  [ HH.label_ [ HH.text "Username: " ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.value username
    , HP.placeholder "Username"
    , HP.required true
    , HE.onValueInput $ Just <<< onInput
    ]
  ]

