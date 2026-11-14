module Domination.UI.DebugLog where

import Prelude

import AppAction (AppAction(..))
import AppState (DebugReport)
import Domination.UI.Css as Css
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall w. DebugReport -> HTML w AppAction
render { text, bugFormUrl } = HH.div
  [ HP.class_ Css.debugLog ]
  [ HH.div
    [ HP.class_ Css.debugLogHeader ]
    [ HH.a
      [ HP.class_ Css.bugFormLink
      , HP.href bugFormUrl
      , HP.target "_blank"
      , HP.rel "noopener noreferrer"
      ]
      [ HH.text "Report Bug" ]
    , HH.button
      [ HP.class_ Css.copyLogButton
      , HE.onClick \_ -> CopyBugReport
      ]
      [ HH.text "Copy bug report" ]
    , HH.button
      [ HE.onClick \_ -> ClearDebugLog ]
      [ HH.text "Clear log" ]
    ]
  , HH.textarea
    [ HP.class_ Css.debugLogContent
    , HP.readOnly true
    , HP.value text
    , HP.attr (HH.AttrName "aria-label") "Bug report"
    ]
  ]
