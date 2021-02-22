module Domination.UI.Util where

import Prelude

import Data.Array ((:))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Domination.UI.Css as Css
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type IncrementerInput i =
  { label :: String
  , mbMin :: Maybe Int
  , mbMax :: Maybe Int
  , value :: Int
  , setValue :: Int -> i
  }

chooseOne
  :: forall w i
  . HTML w i
  -> Array { clickEvent :: i, text :: HTML w i }
  -> HTML w i
chooseOne title bs = HH.div_ $
  title : map mkButton bs
  where
    mkButton { clickEvent, text } = HH.button
      [ HE.onClick \_ -> Just clickEvent ]
      [ text ]

acknowledge
  :: forall w i
  . HTML w i
  -> i
  -> HTML w i
acknowledge message clickEvent = HH.div
  [ HP.class_ Css.dialogue ]
  [ HH.div_ [ message ]
  , HH.button
    [ HE.onClick \_ -> Just clickEvent ]
    [ HH.text "OK" ]
  ]

incrementer :: forall w i. IncrementerInput i -> HTML w i
incrementer { label, mbMin, mbMax, value, setValue } = HH.div
  [ HP.class_ $ ClassName "container" ]
  [ HH.label_ [ HH.text label ]
  , HH.div
    [ HP.class_ $ Css.incrementer ]
    [ HH.button
      [ HE.onClick \_ -> case mbMin of
        Just min ->
          if value <= min
          then Just $ setValue min
          else Just $ setValue (value - 1)
        Nothing -> Just $ setValue (value - 1)
      ] [ HH.text "-" ]
    , HH.input $
      [ HP.value $ show value
      , HP.required true
      , HP.disabled true
      ]
      <> case mbMin of
        Just min -> [ HP.min $ toNumber min ]
        Nothing -> []
      <> case mbMin of
        Just max -> [ HP.max $ toNumber max ]
        Nothing -> []
    , HH.button
      [ HE.onClick $ \_ -> case mbMax of
        Just max ->
          if value >= max
          then Just $ setValue max
          else Just $ setValue (value + 1)
        Nothing -> Just $ setValue (value + 1)
      ]
      [ HH.text "+" ]
    ]
  ]

h1__ :: forall w i. String -> HTML w i
h1__ text = HH.h1_ [ HH.text text ]

h2__ :: forall w i. String -> HTML w i
h2__ text = HH.h2_ [ HH.text text ]

h3__ :: forall w i. String -> HTML w i
h3__ text = HH.h3_ [ HH.text text ]

