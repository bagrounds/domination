module Main where

import Prelude

import Control.Monad
import Data.Lens
import Data.Lens.Getter
import Data.Lens.Record
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Data.Maybe (Maybe(..))
import Data.Array
import Data.Show
import Data.Symbol
import Effect (Effect)
import Halogen (Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

newtype GameState = GameState
  { counter :: Int
  , text :: String
  , players :: Array Player
  }
derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where show = genericShow

newGame :: GameState
newGame = GameState
  { counter: 0
  , text: ""
  , players: [newPlayer, newPlayer]
  }

newtype Player = Player
  { deck :: Array Card
  , hand :: Array Card
  , discard :: Array Card
  , toDiscard :: Array Card
  , atPlay :: Array Card
  }
derive instance genericPlayer :: Generic Player _
instance showPlayer :: Show Player where show = genericShow

play :: Player -> Int -> Maybe Player
play (Player x) i = Player <$> do
  card <- x.hand !! i
  hand' <- deleteAt i x.hand
  let atPlay' = card : x.atPlay
  pure x { hand = hand', atPlay = atPlay' }

newPlayer :: Player
newPlayer = Player
  { deck: concat [(replicate 2 copper), (replicate 3 estate)]
  , hand: replicate 5 copper
  , discard: []
  , toDiscard: []
  , atPlay: []
  }

newtype Card = Card { name :: String, cost :: Int, victoryPoints :: Int, treasure :: Int, buys :: Int, cards :: Int, actions :: Int }
derive instance genericCard :: Generic Card _

instance showCard :: Show Card where show = genericShow

estate :: Card
estate = Card { name: "Estate", cost: 2, victoryPoints: 1, treasure: 0, buys: 0, cards: 0, actions: 0 }
copper :: Card
copper = Card { name: "Copper", cost: 0, victoryPoints: 0, treasure: 1, buys: 0, cards: 0, actions: 0 }

data Action =
  Increment
  | Decrement
  | NewGame
  | Play Int Int

component :: forall a b c d. Component HTML a b c d
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = newGame

  render :: forall a b. Show a => a -> HTML b Action
  render state =
    HH.div_
      [ HH.h1 [] [ HH.text "Game" ]
      , HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "-" ]
      , HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "+" ]
      , HH.button [ HE.onClick \_ -> Just NewGame ] [ HH.text "New Game" ]
      , HH.div_
        [ HH.h2 [] [ HH.text "Player 0" ]
        , HH.button [ HE.onClick \_ -> Just (Play 0 0) ] [ HH.text "Play 0 0" ]
        ]
      , HH.div_ [ HH.text "Game State" ]
      , HH.div_ [ HH.text $ show state ]
      ]

  handleAction = case _ of
    Increment -> H.modify_ \(GameState state) -> GameState $ (over (prop (SProxy :: SProxy "counter")) ((+) 1)) state
    Decrement -> H.modify_ \(GameState state) -> GameState state { counter = state.counter - 1 }
    NewGame -> H.modify_ \(GameState state) -> newGame
    Play player card -> H.modify_ \(GameState state) -> case f state of
      Nothing -> GameState $ state { text = "error" }
      Just gameState -> gameState
      where
        f state = do
          player' <- state.players !! 0 {-player-}
          player'' <- play player' 0 {- card-}
          players' <- updateAt player player'' state.players
          pure (GameState $ state { players = players', text = "good" })


