module Dominion
  ( GameState(..)
  , Phase(..)
  , Player
  , Supply
  , Card
  , Stack
  , newGame
  , next
  , cleanup
  , play
  , purchase
  , value
  , allCards
  , score
  , nextPhase
  ) where

import Prelude

import Data.Array (take, drop, filter, length, deleteAt, mapWithIndex, replicate, updateAt, (!!), (:))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)

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

derive instance genericPhase :: Generic Phase _
derive instance eqPhase :: Eq Phase
instance showPhase :: Show Phase where show = genericShow
instance encodeJsonPhase :: EncodeJson Phase where
  encodeJson a = genericEncodeJson a
instance decodeJsonPhase :: DecodeJson Phase where
  decodeJson a = genericDecodeJson a

next :: Phase -> Phase
next Action = Buy
next Buy = Cleanup
next Cleanup = Action

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

nextPhase :: Int -> GameState -> Maybe GameState
nextPhase playerIndex state =
    if playerIndex == state.turn
    then Just $ state
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
    else Nothing

purchase :: Int -> Player -> Stack -> GameState -> Maybe GameState
purchase playerIndex player stack state =
    if playerIndex == state.turn
    then
      if state.phase == Buy
      then do
        (Tuple player' stack') <- purchase'
        players' <- updateAt playerIndex player' state.players
        let supply' = stack' : (filter ((/=) stack) state.supply)
        pure state { players = players', supply = supply' }
      else Nothing
    else Nothing
  where
    purchase' :: Maybe (Tuple Player Stack)
    purchase' =
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

play :: Int -> Int -> GameState -> Maybe GameState
play player card state =
  if player == state.turn
  then do
    player' <- state.players !! player
    player'' <- play' player' card
    players' <- updateAt player player'' state.players
    pure $ state { players = players' }
  else Nothing
    where
      play' :: Player -> Int -> Maybe Player
      play' x i = do
        card' <- x.hand !! i
        hand' <- deleteAt i x.hand
        let atPlay' = card' : x.atPlay
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

cleanup :: Player -> Player
cleanup player = let
  discard' = player.discard <> player.atPlay <> player.hand <> player.toDiscard <> player.buying
  shouldShuffle = length player.deck < 5
  discard'' = if shouldShuffle then [] else discard'
  deck' = if shouldShuffle then player.deck <> discard' else player.deck
  hand' = take 5 deck'
  deck'' = drop 5 deck' in
  player { deck = deck'', hand = hand', discard = discard'', atPlay = [], buying = [], toDiscard = [] }

allCards :: Player -> Array Card
allCards player = player.hand <> player.deck <> player.atPlay <> player.discard <> player.toDiscard <> player.buying

score :: Player -> Int
score player = foldr (+) 0 $ map _.victoryPoints (allCards player)

