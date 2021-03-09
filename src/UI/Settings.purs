module Domination.UI.Settings where

import Prelude

import AppAction (AppAction(..))
import AppState (AppState)
import Data.Array (mapWithIndex)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log)
import Domination.UI.Card as Card
import Domination.UI.Css as Css
import Domination.UI.DomSlot (Area(..), DomSlot(..))
import Domination.UI.Icons as Icons
import Domination.UI.RenderText (renderText)
import Domination.UI.UsernameInput as UsernameInput
import Domination.UI.Util as Util
import Halogen.Component (ComponentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Version (version)

render
  :: forall t1 t2 t3 m
  . Dom m
  => Log m
  => AppState
  -> HTML (ComponentSlot HTML (description :: Slot t1 t2 DomSlot | t3) m AppAction) AppAction
render cs@{ showMenu, dominationConfig } = let
  { nextPlayerIndex
  , nextPlayerCount
  , longGame
  , kingdom
  } = dominationConfig
  in HH.div
  [ HP.classes
    [ Css.settingsMenu
    , if showMenu
      then Css.showing
      else Css.collapsed
    ]
  ]
  [ HH.div
    [ HP.class_ Css.backButtonContainer ]
    [ HH.button
      [ HE.onClick \_ -> Just $ ToggleMenu , HP.class_ Css.backButton
      ]
      [ HH.text "Back" ]
    ]
  , renderText version
  , UsernameInput.render { onInput: WriteUsername, state: cs }
  , Util.incrementer
    { label: "Players: "
    , mbMin: Just one
    , mbMax: Nothing
    , value: nextPlayerCount
    , setValue: WritePlayerCount
    }
  , Util.incrementer
    { label: "Player #: "
    , mbMin: Just one
    , mbMax: Nothing
    , value: nextPlayerIndex + one
    , setValue: (_ - one) >>> WritePlayerIndex
    }
  , HH.div_
    [ HH.button
      [ HE.onClick \_ -> Just $ StartNewGame
      , HP.class_ Css.newGameButton
      ]
      [ HH.text $ "Start "
        <> (if longGame then "Long " else "Short ")
        <> show nextPlayerCount
        <> " Player Game as Player "
        <> show (nextPlayerIndex + one)
      ]
    , HH.button
      [ HE.onClick \_ -> Just $ LoadGameRequest
      , HP.class_ Css.loadGameButton
      ]
      [ HH.text "Load Game" ]
    ]
  , HH.div_
    [ HH.label
      [ HP.class_ Css.toggleLongGame ]
      [ HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked longGame
        , HE.onChecked \_ -> Just ToggleLongGame
        ]
      , HH.text "Don't end the game when 3 piles are empty"
      ]
    ]
  , renderKingdom kingdom
  ]

renderKingdom kingdom = HH.div
  [ HP.class_ Css.kingdom ]
  $ renderCard `mapWithIndex` kingdom
  where
    renderCard i { card, selected } =
      Card.render (onClick i) (extraClasses selected) card (slot i)
    onClick i _ = Just
      $ ChooseKingdom
      $ ((ix i) <<< prop (SProxy :: SProxy "selected") %~ not) kingdom
    slot = CardSlot KingdomConfigArea
    extraClasses selected = [
      if selected
      then Css.active
      else Css.inactive
    ]

