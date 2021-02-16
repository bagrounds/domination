module Domination.UI.Hud where

import Prelude

import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Domination.Data.GameState (GameState)
import Domination.Data.Player as Player
import Domination.UI.Css as Css
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
  , state :: GameState
  }

render :: forall w i. ActiveState -> HTML w i
render { i, playerIndex, state } =
  case state.players !! playerIndex of
    Nothing -> HH.text $ "cannot find player " <> show playerIndex
      <> " in players: " <> show state.players
    Just player -> HH.div
      [ HP.class_ $ ClassName "hud" ]
        [ HH.div_
          [ HH.h1 [ HP.class_ Css.title ] [ HH.text "Domination" ]
          , HH.h1
            [ HP.class_ Css.playerName ]
            [ HH.text $ "Player " <> show (playerIndex + one) ]
          , HH.h2
            [ HP.class_ Css.iteration ]
            [ HH.text $ "(" <> show i <> ")" ]
          ]
        , HH.ul [ HP.class_ Css.handInfos ] $
          [ HH.li
            [ HP.class_ Css.handInfo ]
            [ HH.text $ show $ length player.deck
            , Icons.cards
            ]
          , HH.li
            [ HP.class_ Css.handInfo ]
            [ renderText player.actions
            ]
          , HH.li
            [ HP.class_ Css.handInfo ]
            [ HH.text $ show $ Player.cash player
            , Icons.money
            ]
          , HH.li
            [ HP.class_ Css.handInfo ]
            [ renderText player.buys
            ]
          , HH.li
            [ HP.class_ Css.handInfo ]
            [ HH.text $ show $ length player.discard
            , Icons.cards
            ]
          ]
        ]

