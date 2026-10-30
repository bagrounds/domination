module Domination.UI.DebugLog where

import Prelude

import AppAction (AppAction(..))
import Domination.UI.Css as Css
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall w. String -> HTML w AppAction
render report = HH.div
  [ HP.class_ Css.debugLog ]
  [ HH.p_
    [ HH.text $ "Found a bug? Copy this report and paste it into a bug report."
      <> " It includes recent app activity and errors from this device."
    ]
  , HH.div
    [ HP.class_ Css.debugLogHeader ]
    [ HH.button
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
    , HP.value report
    , HP.attr (HH.AttrName "aria-label") "Bug report"
    ]
  ]
