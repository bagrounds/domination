--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| It provides a settings UI component for a Domination board game.
--|
--| ### Key Concepts
--| * **Halogen Component**: A functional programming model for building web applications.
--| * **State Management**: The use of `AppState` and `AppAction` to manage the application's state and handle user interactions.
--| * **Dom Slot and Rendering**: The use of `ComponentSlot` and `render` function to define the UI structure and render the component.

module Domination.UI.Settings where

import Prelude

import AppAction (AppAction(..), CardSpecSelection)
import AppState (AppState, SettingsTab(..), defaultKingdom, maxBotDelay, settingsTabLabel, settingsTabs)
import Data.Array (elem, length, mapWithIndex)
import Data.Int (toNumber, round)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Maybe (Maybe(..), maybe)
import Domination.Capability.Dom (class Dom)
import Domination.Capability.Log (class Log)
import Domination.Data.AI.Strategy (Strategy(..), allStrategies, botName)
import Domination.Data.Card (Card, CardSpec, _card, _requirements)
import Domination.Data.Room (Room(..), roomHash, unRoomCode)
import Domination.UI.Card as Card
import Domination.UI.Css as Css
import Domination.UI.DebugLog as DebugLog
import Domination.UI.DomSlot (Area(..), DomSlot(..))
import Domination.UI.RenderText (renderText)
import Domination.UI.ServerUrlInput as ServerUrlInput
import Domination.UI.UsernameInput as UsernameInput
import Domination.UI.Util as Util
import Halogen.Component (ComponentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML (ClassName, HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Version (version)
import Web.UIEvent.MouseEvent (MouseEvent)

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
render cs@{ showMenu, dominationConfig, settingsTab } = let
  { nextPlayerIndex
  , nextPlayerCount
  , longGame
  , kingdom
  , botStrategies
  , botDelay
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
  , renderTabs settingsTab
  , renderTabContent settingsTab cs
    { nextPlayerIndex
    , nextPlayerCount
    , longGame
    , kingdom
    , botStrategies
    , botDelay
    }
  ]

renderTabs :: forall w. SettingsTab -> HTML w AppAction
renderTabs activeTab = HH.div
  [ HP.class_ Css.settingsTabs
  , HP.attr (HH.AttrName "role") "tablist"
  ]
  $ tabButton <$> settingsTabs
  where
    tabButton tab = HH.button
      [ HP.classes $ [ Css.settingsTab ]
        <> if tab == activeTab then [ Css.settingsTabActive ] else []
      , HP.attr (HH.AttrName "role") "tab"
      , HP.attr (HH.AttrName "aria-selected") $ show (tab == activeTab)
      , HE.onClick \_ -> SwitchSettingsTab tab
      ]
      [ HH.text $ settingsTabLabel tab ]

renderTabContent
  :: forall a b r m
  . Dom m
  => Log m
  => SettingsTab
  -> AppState
  -> { nextPlayerIndex :: Int
     , nextPlayerCount :: Int
     , longGame :: Boolean
     , kingdom :: Array CardSpecSelection
     , botStrategies :: Array Strategy
     , botDelay :: Int
     }
  -> HTML
    ( ComponentSlot
      (description :: Slot a b DomSlot | r) m AppAction
    ) AppAction
renderTabContent ConnectionTab cs _ = HH.div_
  [ ServerUrlInput.render { onInput: WriteServerUrl, state: cs }
  , UsernameInput.render { onInput: WriteUsername, state: cs }
  ]
renderTabContent GameSetupTab cs { nextPlayerIndex, nextPlayerCount, longGame, botStrategies } = HH.div_
  [ renderRoomPanel cs
  , Util.incrementer
    { label: "Players: "
    , mbMin: Just $ length botStrategies + one
    , mbMax: Nothing
    , value: nextPlayerCount
    , setValue: WritePlayerCount
    }
  , Util.incrementer
    { label: "Player #: "
    , mbMin: Just one
    , mbMax: Just nextPlayerCount
    , value: nextPlayerIndex + one
    , setValue: (_ - one) >>> WritePlayerIndex
    }
  , HH.p_ [ HH.text $ seatingSummary nextPlayerCount (length botStrategies) ]
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
  ]
renderTabContent BotsTab _ { botStrategies, botDelay } = HH.div_
  [ renderBotControls botStrategies
  , renderBotDelay botDelay
  ]
renderTabContent DebugTab cs _ = DebugLog.render cs.debugReport
renderTabContent KingdomTab _ { kingdom } = HH.div_
  [ HH.button
    [ HE.onClick \_ -> ChooseKingdom ((prop (Proxy :: Proxy "selected") %~ not) <$> defaultKingdom) ]
    [ HH.text "Select None" ]
  , HH.button
    [ HE.onClick \_ -> RandomizeKingdom ]
    [ HH.text "Randomize Kingdom" ]
  , HH.button
    [ HE.onClick \_ -> ChooseKingdom defaultKingdom ]
    [ HH.text "Select All" ]
  , renderKingdom kingdom
  ]

type RoomPanelState r =
  { room :: Room
  , roomNotice :: Maybe String
  , inviteBaseUrl :: String
  , connectionCount :: Int
  | r
  }

renderRoomPanel :: forall w r. RoomPanelState r -> HTML w AppAction
renderRoomPanel { room, roomNotice, inviteBaseUrl, connectionCount } = HH.div
  [ HP.class_ Css.roomPanel ]
  $ renderRoom room <> maybe [] (pure <<< renderInvalidRoomLink) roomNotice
  where
    renderRoom = case _ of
      GlobalRoom ->
        [ HH.p_ [ HH.text "Room: Global (everyone without an invite link)" ]
        , HH.button
          [ HE.onClick \_ -> CreateRoom ]
          [ HH.text "Create private game" ]
        ]
      GameRoom code ->
        [ HH.p_ [ HH.text $ "Room: " <> unRoomCode code <> " · " <> othersHere connectionCount ]
        , HH.label_
          [ HH.text "Invite link: "
          , HH.input
            [ HP.class_ Css.inviteLink
            , HP.readOnly true
            , HP.value $ inviteBaseUrl <> roomHash room
            ]
          ]
        , HH.div_
          [ HH.button
            [ HE.onClick \_ -> CopyInviteLink ]
            [ HH.text "Copy invite link" ]
          , HH.button
            [ HE.onClick \_ -> LeaveRoom ]
            [ HH.text "Leave room" ]
          ]
        , HH.p
          [ HP.class_ Css.roomNotice ]
          [ HH.text "Anyone with this link can join and see the game." ]
        ]

    renderInvalidRoomLink fragment = HH.p
      [ HP.class_ Css.roomNotice ]
      [ HH.text $ "Ignored an invalid room link (#" <> fragment <> "). You are in the global room." ]

othersHere :: Int -> String
othersHere = case _ of
  1 -> "1 other here"
  n -> show n <> " others here"

renderBotDelay :: forall w. Int -> HTML w AppAction
renderBotDelay delay = HH.div
  [ HP.class_ Css.botDelay ]
  [ HH.label_
    [ HH.text $ "Bot play delay: " <> showSeconds delay
    , HH.input
      [ HP.type_ HP.InputRange
      , HP.min 0.0
      , HP.max $ toNumber maxBotDelay
      , HP.step $ HP.Step 250.0
      , HP.value $ show delay
      , HE.onValueInput \s -> WriteBotDelay (round (readNumber s))
      ]
    ]
  , HH.p_
    [ HH.text $ "Each bot play is announced for this long before it happens,"
      <> " so you can undo past bot turns. At 0s bots play instantly."
    ]
  ]

seatingSummary :: Int -> Int -> String
seatingSummary playerCount botCount =
  "You + " <> show botCount <> " bot(s) + "
    <> show (playerCount - botCount - one) <> " remote seat(s)"

showSeconds :: Int -> String
showSeconds ms = let
  s = toNumber ms / 1000.0
  in show s <> "s"

foreign import readNumber :: String -> Number

renderBotControls
  :: forall w
  . Array Strategy
  -> HTML w AppAction
renderBotControls botStrategies = HH.div
  [ HP.class_ Css.botControls ]
  [ HH.label_ [ HH.text "AI Players" ]
  , HH.div_ $ mapWithIndex renderBot botStrategies
  , HH.div
    [ HP.class_ Css.addBot ]
    $ addBotButton <$> allStrategies
  ]
  where
    renderBot :: Int -> Strategy -> HTML w AppAction
    renderBot i strategy = HH.div
      [ HP.class_ Css.botEntry ]
      [ HH.span_ [ HH.text $ botName strategy ]
      , HH.button
        [ HE.onClick \_ -> RemoveBot i
        , HP.class_ Css.removeBot
        ]
        [ HH.text "✕" ]
      ]

    addBotButton :: Strategy -> HTML w AppAction
    addBotButton strategy = HH.button
      [ HE.onClick \_ -> AddBot strategy
      , HP.class_ Css.addBotButton
      ]
      [ HH.text $ "+ " <> botName strategy ]

type RenderedKingdom a b r m = HTML
    ( ComponentSlot
      (description :: Slot a b DomSlot | r) m AppAction
    ) AppAction

renderKingdom
  :: forall a b r m
  . Dom m
  => Log m
  => Array CardSpecSelection
  -> RenderedKingdom a b r m
renderKingdom kingdom = HH.div
  [ HP.class_ Css.kingdom ]
  $ renderCard `mapWithIndex` kingdom
  where
    renderCard :: Int -> CardSpecSelection -> RenderedKingdom a b r m
    renderCard i { cardSpec, selected } =
      Card.render (onClick i cardSpec selected) (extraClasses selected) (_card cardSpec) (slot i)

    onClick :: Int -> CardSpec -> Boolean -> MouseEvent -> AppAction
    onClick i cardSpec selected _ = ChooseKingdom (newKingdom)
      where
        newKingdom :: Array CardSpecSelection
        newKingdom = selectRequirements $ toggleSelected kingdom
        toggleSelected :: Array CardSpecSelection -> Array CardSpecSelection
        toggleSelected = (ix i) <<< prop (Proxy :: Proxy "selected") %~ not
        requirements :: Array Card
        requirements = _requirements cardSpec
        selectRequirements :: Array CardSpecSelection -> Array CardSpecSelection
        selectRequirements selections = selectRequirement <$> selections
        selectRequirement :: CardSpecSelection -> CardSpecSelection
        selectRequirement { cardSpec: cs, selected: s } =  { cardSpec: cs, selected: s || (not selected && elem (_card cs) requirements) }

    slot :: Int -> DomSlot
    slot = CardSlot KingdomConfigArea

    extraClasses :: Boolean -> Array ClassName
    extraClasses selected =
      [ if selected
        then Css.active
        else Css.inactive
      ]
