module Domination.UI.Hud where

import Prelude

import Data.Array.NonEmpty (mapWithIndex, toArray, (!!))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Domination.Data.Card as Card
import Domination.Data.Game (Game)
import Domination.Data.Phase (Phase(..))
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.UI.Css as Css
import Domination.UI.Domination.Action (Action)
import Domination.UI.Icons as Icons
import Domination.UI.RenderText (renderText)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- TODO: move ActiveState to own module to prevent cycle/duplication
type ActiveState =
  { i :: Int
  , playerIndex :: Int
  , playerCount :: Int
  , state :: Game
  , showSupply :: Boolean
  }

render :: forall w. ActiveState -> HTML w Action
render cs@{ i, playerIndex, state } =
  case state.players !! playerIndex of
    Nothing -> HH.text $ "cannot find player " <> show playerIndex
      <> " in players: " <> show state.players
    Just _ -> HH.div
      [ HP.class_ $ ClassName "hud" ]
      [ HH.div
        [ HP.class_ $ ClassName "title-bar" ]
        [ HH.h1 [ HP.class_ Css.title ] [ HH.text "Domination" ]
        , HH.h1
          [ HP.class_ Css.playerName ]
          [ HH.text $ "Player " <> show (playerIndex + one) ]
        , HH.span
          [ HP.class_ Css.iteration ]
          [ HH.text $ "(" <> show i <> ")" ]
        ]
        , renderStats cs
        ]

renderHandInfos :: forall w. ActiveState -> HTML w Action
renderHandInfos { playerIndex, state } =
  case state.players !! playerIndex of
    Nothing -> HH.text $ "cannot find player " <> show playerIndex
      <> " in players: " <> show state.players
    Just player -> HH.ul
      [ HP.class_ Css.handInfos ] $
      [ HH.li
        [ HP.class_ Css.handInfo ]
        [ HH.span_
          [ HH.text $ show $ (length player.deck :: Int)
          , Icons.cards
          ]
        ]
      , HH.li
        ( [ HP.classes $
            [ Css.handInfo ] <>
              ( if playerIndex == state.turn
                && state.phase == ActionPhase
                then [ Css.drawAttention ]
                else []
              )
          ]
        )
        [ renderText player.actions ]
      , HH.li
        ( [ HP.classes $
            [ Css.handInfo ] <>
              ( if playerIndex == state.turn
                && state.phase == BuyPhase
                then [ Css.drawAttention ]
                else []
              )
          ]
        )
        [ HH.span_
          [ HH.text $ show $ Player.cash player
          , Icons.money
          ]
        ]
      , HH.li
        ( [ HP.classes $
            [ Css.handInfo ] <>
              ( if playerIndex == state.turn
                && state.phase == BuyPhase
                then [ Css.drawAttention ]
                else []
              )
          ]
        )
        [ renderText player.buys
        ]
      , HH.li
        [ HP.class_ Css.handInfo ]
        [ HH.span_
          [ HH.text $ show $ (length player.discard :: Int)
          , Icons.cards
          ]
        ]
      ]

renderStats :: forall w i. ActiveState -> HTML w i
renderStats cs = HH.div
  [ HP.class_ Css.statsContainer ]
  $ toArray
  $ playerStats cs `mapWithIndex` cs.state.players

playerStats
  :: forall w i
  . ActiveState
  -> Int
  -> Player
  -> HTML w i
playerStats { state } playerIndex player =
  HH.ul
    [ HP.class_ Css.stats ]
    [ HH.li
      [ HP.class_ Css.currentPlayer ]
      [ HH.span_
        [ if state.turn == playerIndex
          then case state.phase of
            ActionPhase -> Icons.actions
            BuyPhase -> Icons.buys
            CleanupPhase -> Icons.cards
          else Icons.empty
        , HH.text $ "Player " <> show (playerIndex + one)
        ]
      ]
    , HH.li
      [ HP.class_ Css.stat ]
      [ renderText player.actions ]
    , HH.li
      [ HP.class_ Css.stat ]
      [ HH.span_
        [ HH.text
          case state.phase, playerIndex of
            ActionPhase, i | i == state.turn ->
              show $ Card.value player.atPlay
            BuyPhase, i | i == state.turn ->
              show $ Player.cash player
            _, _ -> "_"
        , Icons.money
        ]
      ]
    , HH.li
      [ HP.class_ Css.stat ]
      [ renderText player.buys ]
    , HH.li
      [ HP.class_ Css.stat ]
      [ renderText $ Player.score player
      ]
    ]
