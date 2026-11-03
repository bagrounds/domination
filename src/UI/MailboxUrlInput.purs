module Domination.UI.MailboxUrlInput where

import Prelude

import Domination.UI.Css as Css
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State r =
  { mailboxUrl :: String
  | r
  }

type RenderInput r1 r a =
  { state :: State r
  , onInput :: String -> a
  | r1
  }

render :: forall s r a r1. RenderInput r1 r a -> HTML s a
render { onInput, state: { mailboxUrl } } = HH.div
  [ HP.class_ Css.mailboxUrlInput ]
  [ HH.label_ [ HH.text "Mailbox URL: " ]
  , HH.input
    [ HP.type_ HP.InputText
    , HP.value mailboxUrl
    , HP.placeholder "empty turns async play off"
    , HP.title "Where room games wait for players who are away. Empty turns async play off."
    , HE.onValueInput $ onInput
    ]
  ]
