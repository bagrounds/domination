module Dominion
  ( GameState(..)
  , Phase(..)
  , Player
  , Supply
  , Card
  , CardType
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
  , cash
  , isTreasure
  , isAction
  ) where

import Prelude

import Control.Apply (lift3)
import Data.Array (notElem, take, drop, filter, length, deleteAt, mapWithIndex, replicate, updateAt, (!!), (:))
import Data.Foldable (class Foldable, foldr, any, null)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..), fst)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)

import Effect (Effect)
import Effect.Random (randomInt)

type GameState =
  { turn :: Int
  , phase :: Phase
  , text :: String
  , players :: Array Player
  , supply :: Supply
  }

type Supply = Array Stack
type Stack = { card :: Card, count :: Int }

data Phase = ActionPhase | BuyPhase | CleanupPhase

derive instance genericPhase :: Generic Phase _
derive instance eqPhase :: Eq Phase
instance showPhase :: Show Phase where show = genericShow
instance encodeJsonPhase :: EncodeJson Phase where
  encodeJson a = genericEncodeJson a
instance decodeJsonPhase :: DecodeJson Phase where
  decodeJson a = genericDecodeJson a

data CardType = Action | Treasure | Victory

derive instance genericCardType :: Generic CardType _
derive instance eqCardType :: Eq CardType
instance showCardType :: Show CardType where show = genericShow
instance encodeJsonCardType :: EncodeJson CardType where
  encodeJson a = genericEncodeJson a
instance decodeJsonCardType :: DecodeJson CardType where
  decodeJson a = genericDecodeJson a

next :: Phase -> Phase
next ActionPhase = BuyPhase
next BuyPhase = CleanupPhase
next CleanupPhase = ActionPhase

newGame :: GameState
newGame =
  { turn: 0
  , phase: ActionPhase
  , text: ""
  , players: [newPlayer, newPlayer]
  , supply:
    [ { card: copper, count: 50 }
    , { card: silver, count: 50 }
    , { card: gold, count: 50 }
    , { card: platinum, count: 50 }
    , { card: estate, count: 8 }
    , { card: duchy, count: 8 }
    , { card: province, count: 8 }
    , { card: colony, count: 8 }
    , { card: greatHall, count: 8 }
    , { card: village, count: 10 }
    , { card: woodCutter, count: 10 }
    , { card: monument, count: 8 }
    , { card: smithy, count: 10 }
    , { card: workersVillage, count: 10 }
    , { card: bazaar, count: 10 }
    , { card: festival, count: 10 }
    , { card: laboratory, count: 10 }
    , { card: market, count: 10 }
    , { card: harem, count: 8 }
    ]
  }

type Player =
  { deck :: Array Card
  , hand :: Array Card
  , discard :: Array Card
  , toDiscard :: Array Card
  , atPlay :: Array Card
  , buying :: Array Card
  , actions :: Int
  , buys :: Int
  }

value :: Array Card -> Int
value = foldr (+) 0 <<< map _.treasure

cost :: Array Card -> Int
cost = foldr (+) 0 <<< map _.cost

nextPhase :: Int -> GameState -> Maybe GameState
nextPhase playerIndex state =
    if playerIndex == state.turn
    then Just $ state
      { phase = next state.phase
      , turn =
        if state.phase == CleanupPhase
        then (state.turn + 1) `mod` (length state.players)
        else state.turn
      , players =
          if state.phase == CleanupPhase
          then mapWithIndex (\i p -> if i == playerIndex then cleanup p else p) state.players
          else state.players
      }
    else Nothing

purchase :: Int -> Player -> Stack -> GameState -> Maybe GameState
purchase playerIndex player stack state =
    if playerIndex /= state.turn
    then Nothing
    else
      if state.phase /= BuyPhase
      then Nothing
      else
        if player.buys < 1
        then Nothing
        else do
          (Tuple player' stack') <- purchase'
          players' <- updateAt playerIndex player' state.players
          let supply' = (\s -> if s == stack then stack' else s) <$> state.supply
          pure state { players = players', supply = supply' }
  where
    purchase' :: Maybe (Tuple Player Stack)
    purchase' =
      if stack.count == 0
      then Nothing
      else
        if cash player < stack.card.cost
        then Nothing
        else Just (Tuple
          (player { buying = stack.card : player.buying, buys = player.buys - 1 })
          (stack { count = stack.count - 1 }))

isTreasure :: Card -> Boolean
isTreasure = contains Treasure <<< _.types
isAction :: Card -> Boolean
isAction = contains Action <<< _.types

cash :: Player -> Int
cash player = (value player.atPlay)
  + (value $ isTreasure `filter` player.hand)
  - (cost player.buying)

contains :: forall a f. Eq a => Foldable f => a -> f a -> Boolean
contains x xs = ((==) x) `any` xs

play :: Int -> Int -> GameState -> Effect (Maybe GameState)
play player card state =
  if player == state.turn
  then case (state.players !! player) of
      Nothing -> pure Nothing
      Just player' -> do
        (player'' :: Maybe Player) <- play' player' card
        pure $ case player'' of
          Nothing -> Nothing
          Just player''' -> (\p -> state { players = p })
            <$> (updateAt player player''' state.players)
  else pure Nothing
    where
      play' :: Player -> Int -> Effect (Maybe Player)
      play' p i =
        case (do
          card' <- p.hand !! i
          hand' <- deleteAt i p.hand
          pure $ Tuple card' hand') of
          Nothing -> pure Nothing
          Just (Tuple c h) -> result c h
          where
            result :: Card -> Array Card -> Effect (Maybe Player)
            result card' hand' = do
              p' <- drawCards card'.cards p { hand = hand' }
              let atPlay' = card' : p'.atPlay
              if ((==) Action) `any` card'.types
                then pure $ Just p'
                  { atPlay = atPlay'
                  , actions = p.actions + card'.actions - 1
                  , buys = p.buys + card'.buys
                  }
                else pure Nothing

drawCards :: Int -> Player -> Effect Player
drawCards n p = if n > 0
  then drawCards (n - 1) =<< (drawCard p)
  else pure p

drawCard :: Player -> Effect Player
drawCard player =
  if null player.deck
    then do
    (_{ discard = [] } <<< draw player.hand) <$> (shuffle player.discard)
    else pure $ draw player.hand player.deck
  where
    draw h d = let
      h' = take 1 d <> h
      d' = drop 1 d
      in player { hand = h', deck = d' }

newPlayer :: Player
newPlayer =
  { deck: []
  , hand: []
  , discard: (replicate 5 copper) <> (replicate 3 estate)
  , toDiscard: []
  , atPlay: []
  , buying: []
  , actions: 1
  , buys: 1
  }

type Card =
  { types :: Array CardType
  , name :: String
  , cost :: Int
  , victoryPoints :: Int
  , treasure :: Int
  , buys :: Int
  , cards :: Int
  , actions :: Int
  }

treasure :: Card
treasure = { types: [Treasure], name: "", cost: 0, victoryPoints: 0, treasure: 0, buys: 0, cards: 0, actions: 0 }
victory :: Card
victory = treasure { types = [Victory] }
action :: Card
action = treasure { types = [Action] }
copper :: Card
copper = treasure { name = "Copper", treasure = 1 }
silver :: Card
silver = treasure { name = "Silver", cost = 3, treasure = 2 }
gold :: Card
gold = treasure { name = "Gold", cost = 6, victoryPoints = 0, treasure = 3 }
platinum :: Card
platinum = treasure { name = "Platinum", cost = 9, treasure = 5 }
estate :: Card
estate = victory { name = "Estate", cost = 2, victoryPoints  = 1 }
duchy :: Card
duchy = victory { name = "Duchy", cost = 5, victoryPoints = 3 }
province :: Card
province = victory { name = "Province", cost = 8, victoryPoints = 6 }
colony :: Card
colony = victory { name = "Colony", cost = 11, victoryPoints = 10 }
greatHall :: Card
greatHall = victory { types = [Action, Victory], name = "Great Hall", cost = 3, cards = 1, actions = 1, victoryPoints = 1 }
village :: Card
village = action { name = "Village", cost = 3, cards = 1, actions = 2 }
woodCutter :: Card
woodCutter = action { name = "Wood Cutter", cost = 3, buys = 1, treasure = 2 }
laboratory :: Card
laboratory = action { name = "Laboratory", cost = 5, cards = 2, actions = 1 }
smithy :: Card
smithy = action { name = "Smithy", cost = 4, cards = 3 }
festival :: Card
festival = action { name = "Festival", cost = 5, actions = 2, buys = 1, treasure = 2 }
market :: Card
market = action { name = "Market", cost = 5, actions = 1, cards = 1, buys = 1, treasure = 1 }
harem :: Card
harem = treasure { types = [Treasure, Victory], name = "Harem", cost = 6, treasure = 2, victoryPoints = 2 }
bazaar :: Card
bazaar = action { name = "Bazaar", cost = 5, cards = 1, actions = 2, treasure = 1 }
monument :: Card
monument = action { types = [Action, Victory], name = "Monument", cost = 4, treasure = 2, victoryPoints = 1 }
workersVillage :: Card
workersVillage = action { name = "Worker's Village", cost = 4, cards = 1, actions = 2, buys = 1 }

cleanup :: Player -> Player
cleanup player = let
  discard' = player.discard <> player.atPlay <> player.hand <> player.toDiscard <> player.buying
  shouldShuffle = length player.deck < 5
  discard'' = if shouldShuffle then [] else discard'
  -- shuffled <- shuffle discard'
  -- let deck' = if shouldShuffle then player.deck <> shuffled else player.deck
  deck' = if shouldShuffle then player.deck <> discard' else player.deck
  hand' = take 5 deck'
  deck'' = drop 5 deck' in
  player { deck = deck'', hand = hand', discard = discard'', atPlay = [], buying = [], toDiscard = [], buys = 1, actions = 1 }

shuffle :: forall a. Eq a => Array a -> Effect (Array a)
shuffle array = fst <$> shuffle' (Tuple [] array)
  where
    shuffle' :: Tuple (Array a) (Array a) -> Effect (Tuple (Array a) (Array a))
    shuffle' (Tuple xs []) = pure $ (Tuple xs [])
    shuffle' (Tuple xs ys) = do
      i <- randomInt 0 (length ys - 1)
      let a1 = take 1 $ drop i array
      let ys' = (flip notElem a1) `filter` ys
      shuffle' (Tuple (a1 <> xs) ys')

allCards :: Player -> Array Card
allCards player = player.hand <> player.deck <> player.atPlay <> player.discard <> player.toDiscard <> player.buying

score :: Player -> Int
score player = foldr (+) 0 $ map _.victoryPoints (allCards player)

