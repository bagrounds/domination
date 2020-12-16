module Main where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Array (take, drop, filter, length, deleteAt, mapWithIndex, replicate, updateAt, (!!), (:))
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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

type GameState =
  { turn :: Int
  , phase :: Phase
  , text :: String
  , players :: Array Player
  , supply :: Supply
  }

type Supply = Array Stack
type Stack = { card :: Card, count :: Int }

data Phase = Action | Buy | Cleanup

derive instance eqPhase :: Eq Phase

next :: Phase -> Phase
next Action = Buy
next Buy = Cleanup
next Cleanup = Action

derive instance genericPhase :: Generic Phase _
instance showPhase :: Show Phase where show = genericShow

newGame :: GameState
newGame =
  { turn: 0
  , phase: Action
  , text: ""
  , players: [newPlayer, newPlayer]
  , supply:
    [ { card: copper, count: 50 }
    , { card: silver, count: 50 }
    , { card: gold, count: 50 }
    , { card: estate, count: 8 }
    , { card: duchy, count: 8 }
    , { card: province, count: 8 }
    ]
  }

type Player =
  { deck :: Array Card
  , hand :: Array Card
  , discard :: Array Card
  , toDiscard :: Array Card
  , atPlay :: Array Card
  , buying :: Array Card
  }

value :: Array Card -> Int
value = foldr (+) 0 <<< map _.treasure

purchase :: Stack -> Player -> Maybe (Tuple Player Stack)
purchase stack player =
  if stack.count == 0
  then Nothing
  else
    if (foldr (+) 1 <<< map _.buys) player.atPlay <= length player.buying
    then Nothing
    else
      if (value player.atPlay) - (value player.buying) < stack.card.cost
      then Nothing
      else Just (Tuple
        (player { buying = stack.card : player.buying })
        (stack { count = stack.count - 1 }))

play :: Player -> Int -> Maybe Player
play x i = do
  card <- x.hand !! i
  hand' <- deleteAt i x.hand
  let atPlay' = card : x.atPlay
  pure x { hand = hand', atPlay = atPlay' }

newPlayer :: Player
newPlayer =
  { deck: (replicate 2 copper) <> (replicate 3 estate)
  , hand: replicate 5 copper
  , discard: []
  , toDiscard: []
  , atPlay: []
  , buying: []
  }

type Card =
  { name :: String
  , cost :: Int
  , victoryPoints :: Int
  , treasure :: Int
  , buys :: Int
  , cards :: Int
  , actions :: Int
  }

estate :: Card
estate = { name: "Estate", cost: 2, victoryPoints: 1, treasure: 0, buys: 0, cards: 0, actions: 0 }
duchy :: Card
duchy = { name: "Duchy", cost: 5, victoryPoints: 3, treasure: 0, buys: 0, cards: 0, actions: 0 }
province :: Card
province = { name: "Province", cost: 8, victoryPoints: 6, treasure: 0, buys: 0, cards: 0, actions: 0 }
copper :: Card
copper = { name: "Copper", cost: 0, victoryPoints: 0, treasure: 1, buys: 0, cards: 0, actions: 0 }
silver :: Card
silver = { name: "Silver", cost: 3, victoryPoints: 0, treasure: 2, buys: 0, cards: 0, actions: 0 }
gold :: Card
gold = { name: "Gold", cost: 6, victoryPoints: 0, treasure: 3, buys: 0, cards: 0, actions: 0 }

component :: forall a b c d. Component HTML a b c d
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: b -> GameState
  initialState _ = newGame

render :: forall b. GameState -> HTML b Action
render state =
  HH.div_
    [ HH.h1 [] [ HH.text "Domination" ]
    , HH.button [ HE.onClick \_ -> Just NewGame ] [ HH.text "New Game" ]
    , HH.div_ $ renderPlayers state
    , HH.div_ [ HH.h2 [] [ HH.text "Game State" ] ]
    , HH.div_ [ HH.text $ show state ]
    ]

renderSupply :: forall a. Int -> Player -> GameState -> Array (HTML a Action)
renderSupply playerIndex player state =
  map (renderCardInSupply playerIndex player) state.supply

renderCardInSupply :: forall a. Int -> Player -> Stack -> HTML a Action
renderCardInSupply playerIndex player stack = HH.button [ HE.onClick \_ -> Just $ Purchase playerIndex player stack ] [ HH.text stack.card.name ]

renderPlayers :: forall a. GameState -> Array (HTML a Action)
renderPlayers state =
  [ HH.p [] [ HH.text $ "Turn: Player " <> show state.turn ]
  , HH.p [] [ HH.text $ "Phase: " <> show state.phase ]
  ]
  <> (renderPlayer state) `mapWithIndex` state.players

renderPlayer :: forall a. GameState -> Int -> Player -> HTML a Action
renderPlayer state playerIndex player = HH.div_
  [ HH.h2 [] [ HH.text $ "Player " <> (show playerIndex) ]
  , HH.button [ HE.onClick \_ -> Just $ NextPhase playerIndex ] [ HH.text "Next Phase" ]
  , HH.h3 [] [ HH.text $ "Supply" ]
  , HH.div_ $ renderSupply playerIndex player state
  , HH.h3 [] [ HH.text $ "Cards in Hand" ]
  , HH.div_ $ mapWithIndex renderCardInHand (map (\x -> Tuple playerIndex x) player.hand)
  , HH.h3 [] [ HH.text $ "Cards at Play" ]
  , HH.div_ $ map renderCardAtPlay player.atPlay
  ]

renderCardAtPlay :: forall a. Card -> HTML a Action
renderCardAtPlay card = HH.label_ [HH.text card.name]

renderCardInHand :: forall a. Int -> Tuple Int Card -> HTML a Action
renderCardInHand cardIndex (Tuple playerIndex card) = HH.button [HE.onClick \_ -> Just (Play playerIndex cardIndex)] [ HH.text card.name ]

data Action =
  NewGame
  | NextPhase Int
  | Play Int Int
  | Purchase Int Player Stack

cleanup :: Player -> Player
cleanup player = let
  discard' = player.discard <> player.atPlay <> player.hand <> player.toDiscard <> player.buying
  shouldShuffle = length player.deck < 5
  discard'' = if shouldShuffle then [] else discard'
  deck' = if shouldShuffle then player.deck <> discard' else player.deck
  hand' = take 5 deck'
  deck'' = drop 5 deck' in
  player { deck = deck'', hand = hand', discard = discard'', atPlay = [], buying = [], toDiscard = [] }

handleAction :: forall a. MonadState GameState a => Action -> a Unit
handleAction = case _ of
  NewGame -> H.modify_ \state -> newGame
  NextPhase playerIndex -> H.modify_ \state ->
    if playerIndex == state.turn
    then state
      { phase = next state.phase
      , turn =
        if state.phase == Cleanup
        then (state.turn + 1) `mod` (length state.players)
        else state.turn
      , players =
          if state.phase == Cleanup
          then mapWithIndex (\i p -> if i == playerIndex then cleanup p else p) state.players
          else state.players
      }
    else state { text = "Error: not your turn!" }
  Play player card -> H.modify_ \state -> case f state of
    Nothing -> state { text = "error" }
    Just gameState -> gameState
    where
      f state =
        if player == state.turn
        then do
          player' <- state.players !! player
          player'' <- play player' card
          players' <- updateAt player player'' state.players
          pure $ state { players = players', text = "good" }
        else pure state { text = "Error: not your turn!" }
  Purchase playerIndex player stack -> H.modify_ \state ->
    if playerIndex == state.turn
    then
      if state.phase == Buy
      then case (do
        (Tuple player' stack') <- purchase stack player
        players' <- updateAt playerIndex player' state.players
        let supply' = stack' : (filter ((/=) stack) state.supply)
        pure state { players = players', supply = supply', text = "good" })
          of
          Nothing -> state { text = "Error trying to buy card!" }
          Just state -> state
      else state { text = "Error: wrong phase!" }
    else state { text = "Error: not your turn!" }

