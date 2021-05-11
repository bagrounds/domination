module Domination.UI.Settings where

import Prelude

import AppAction (AppAction(..))
import AppState (AppState)
import Data.Array (mapWithIndex)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log)
import Domination.Data.Card (Card)
import Domination.UI.Card as Card
import Domination.UI.Css as Css
import Domination.UI.DomSlot (Area(..), DomSlot(..))
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

renderEmpty
  :: forall a b r m
  . Dom m
  => Log m
  => HTML
    ( ComponentSlot
      (description :: Slot a b DomSlot | r) m AppAction
    ) AppAction
renderEmpty = HH.div
  [ HP.classes
    [ Css.settingsMenu
    , Css.collapsed
    ]
  ]
  []

render
  :: forall a b r m
  . Dom m
  => Log m
  => AppState
  -> HTML
    ( ComponentSlot
      (description :: Slot a b DomSlot | r) m AppAction
    ) AppAction
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
      [ HE.onClick \_ -> ToggleMenu
      , HP.class_ Css.backButton
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
      [ HE.onClick \_ -> StartNewGame
      , HP.class_ Css.newGameButton
      ]
      [ HH.text $ "Start "
        <> (if longGame then "Long " else "Short ")
        <> show nextPlayerCount
        <> " Player Game as Player "
        <> show (nextPlayerIndex + one)
      ]
    , HH.button
      [ HE.onClick \_ -> LoadGameRequest
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
        , HE.onChecked \_ -> ToggleLongGame
        ]
      , HH.text "Don't end the game until the leader can't be beaten"
      ]
    ]
  , HH.button
    [ HE.onClick \_ -> RandomizeKingdom
    , HP.class_ Css.randomizeKingdomButton
    ]
    [ HH.text "Randomize Kingdom" ]
  , renderKingdom kingdom
  ]

renderKingdom
  :: forall a b r m
  . Dom m
  => Log m
  => Array { card :: Card, selected :: Boolean }
  -> HTML
    ( ComponentSlot
      (description :: Slot a b DomSlot | r) m AppAction
    ) AppAction
renderKingdom kingdom = HH.div
  [ HP.class_ Css.kingdom ]
  $ renderCard `mapWithIndex` kingdom
  where
    renderCard i { card, selected } =
      Card.render (onClick i) (extraClasses selected) card (slot i)

    onClick i _ = ChooseKingdom
      $ ((ix i) <<< prop (Proxy :: Proxy "selected") %~ not) kingdom

    slot = CardSlot KingdomConfigArea

    extraClasses selected =
      [ if selected
        then Css.active
        else Css.inactive
      ]

