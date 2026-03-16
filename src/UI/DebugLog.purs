module Domination.UI.DebugLog where

import Prelude

import AppAction (AppAction(..))
import Data.String (joinWith)
import Domination.UI.Css as Css
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall w. Array String -> HTML w AppAction
render entries = HH.div
  [ HP.class_ Css.debugLog ]
  [ HH.div
    [ HP.class_ Css.debugLogHeader ]
    [ HH.span_ [ HH.text "Debug Log" ]
    , HH.button
      [ HP.class_ Css.copyLogButton
      , HE.onClick \_ -> CopyLogs
      ]
      [ HH.text "Copy Debug Log" ]
    ]
  , HH.div
    [ HP.class_ Css.debugLogContent ]
    [ HH.pre_ [ HH.text $ joinWith "\n" entries ] ]
  ]
