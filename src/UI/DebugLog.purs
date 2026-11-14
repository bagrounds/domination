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
  [ HH.p_
    [ HH.text $ "Found a bug? Open the GitHub bug form to describe it."
      <> " It starts with this report, which includes recent app activity and errors from this device."
      <> " Review it and remove anything you'd rather not share, such as your room code, before you submit."
      <> " You'll need a GitHub account."
    ]
  , HH.p_
    [ HH.text $ "A long report is cut to its newest lines to fit in the link."
      <> " To share all of it, copy the report and paste it into the form."
    ]
  , HH.div
    [ HP.class_ Css.debugLogHeader ]
    [ HH.a
      [ HP.class_ Css.bugFormLink
      , HP.href bugFormUrl
      , HP.target "_blank"
      , HP.rel "noopener noreferrer"
      ]
      [ HH.text "Open GitHub bug form" ]
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
