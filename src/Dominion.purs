module Dominion
  ( GameState(..)
  , Phase(..)
  , Player
  , Supply
  , Card
  , CardType
  , Stack
  , Special
  , Target
  , Command
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
  , isAction
  , isTreasure
  , isVictory
  , setup
  , hasActions
  , actionCardsInHand
  , numActionCardsInHand
  , hasActionCardsInHand
  , nextPlayer
  ) where

import Prelude

import Comm as Comm
import Control.Apply (lift2)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array (elem, findIndex, takeWhile, notElem, take, drop, filter, length, deleteAt, mapWithIndex, replicate, updateAt, (!!), (:))
import Data.Foldable (class Foldable, foldr, any, null, foldM)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
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

data CardType = Action | Treasure | Victory | Curse | Attack

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

newGame :: Int -> GameState
newGame i =
  { turn: 0
  , phase: ActionPhase
  , text: ""
  , players: replicate i newPlayer
  , supply:
    [ { card: copper, count: 50 }
    , { card: silver, count: 50 }
    , { card: gold, count: 50 }
    , { card: platinum, count: 50 }
    , { card: estate, count: 8 }
    , { card: duchy, count: 8 }
    , { card: province, count: 8 }
    , { card: colony, count: 8 }
    , { card: curse, count: 10 * (i - 1) }
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
    , { card: witch, count: 10 }
    , { card: councilRoom, count: 10 }
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

nextPhase :: forall m. MonadEffect m => Int -> GameState -> m (Maybe GameState)
nextPhase playerIndex state =
    if playerIndex /= state.turn
    then pure Nothing
    else do
      let phase' = next state.phase
      let turn' = if state.phase == CleanupPhase
        then nextPlayer state
        else state.turn
      players' <- (if state.phase == CleanupPhase
          then sequence $
            mapWithIndex (\i p -> if i == playerIndex then cleanup p else pure p)
            state.players
          else pure state.players)
      pure $ Just state { phase = phase', turn = turn', players = players' }

setup :: forall m. MonadEffect m => GameState -> m GameState
setup gameState = do
  players <- traverse (drawCards 5) gameState.players
  pure gameState { players = players }

nextPlayer :: GameState -> Int
nextPlayer state = (state.turn + 1) `mod` (length state.players)

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

isAction :: Card -> Boolean
isAction = contains Action <<< _.types
isTreasure :: Card -> Boolean
isTreasure = contains Treasure <<< _.types
isVictory :: Card -> Boolean
isVictory = contains Victory <<< _.types

hasActions :: Player -> Boolean
hasActions = (_ > 0) <<< _.actions

actionCardsInHand :: Player -> Array Card
actionCardsInHand = filter isAction <<< _.hand

numActionCardsInHand :: Player -> Int
numActionCardsInHand = length <<< actionCardsInHand

hasActionCardsInHand :: Player -> Boolean
hasActionCardsInHand = (_ > 0) <<< numActionCardsInHand

cash :: Player -> Int
cash player = (value player.atPlay)
  + (value $ isTreasure `filter` player.hand)
  - (cost player.buying)

contains :: forall a f. Eq a => Foldable f => a -> f a -> Boolean
contains x xs = ((==) x) `any` xs

play :: forall m. MonadEffect m => Int -> Int -> GameState -> m (Maybe GameState)
play playerIndex cardIndex state =
  if playerIndex /= state.turn
  then pure Nothing
  else case (state.players !! playerIndex) of
      Nothing -> pure Nothing
      Just player' -> do
        case player'.hand !! cardIndex of
          Nothing -> pure Nothing
          Just card -> do
            (player'' :: Maybe Player) <- play' player' cardIndex
            case player'' of
              Nothing -> pure Nothing
              Just player''' -> do
                state' :: GameState <- applySpecials state playerIndex card
                let maybePlayers = updateAt playerIndex player''' state'.players
                pure $ map (state' { players = _ }) maybePlayers
    where
      applySpecials :: GameState -> Int -> Card -> m GameState
      applySpecials state playerIndex card =
        foldM (applyEffectToTargets playerIndex) state card.specials

      applyEffectToTargets :: Int -> GameState -> Special -> m GameState
      applyEffectToTargets attackerIndex state { target, command } =
        foldM (\state i -> fromMaybe state <$> applyEffectToTarget command state i) state (targetIndices target attackerIndex state)

      applyEffectToTarget :: Command -> GameState -> Int -> m (Maybe GameState)
      applyEffectToTarget (Gain card) state targetIndex = pure do
        target :: Player <- state.players !! targetIndex
        stackIndex <- findIndex (\x -> x.card == card) state.supply
        stack <- state.supply !! stackIndex
        let count' = if stack.count > 0 then stack.count - 1 else stack.count
        let stack' = stack { count = count' }
        supply' <- updateAt stackIndex stack' state.supply
        let target' = if stack.count > 0 then target { discard = stack.card : target.discard } else target
        players' <- updateAt targetIndex target' state.players
        pure (state { players = players', supply = supply' })
      applyEffectToTarget (Draw n) state targetIndex = do
        case state.players !! targetIndex of
          Nothing -> pure Nothing
          Just target -> do
            target' <- drawCards n target
            let players = state.players
            let players' = mapWithIndex (\i p -> if i == targetIndex then target' else p) state.players
            pure $ Just state { players = players' }

      targetIndices :: Target -> Int -> GameState -> Array Int
      targetIndices EveryoneElse attackerIndex state = (_ /= attackerIndex) `filter` indices state.players
      targetIndices Everyone _ state = indices state.players

      play' :: Player -> Int -> m (Maybe Player)
      play' player cardIndex =
        case lift2 Tuple (player.hand !! cardIndex) (deleteAt cardIndex player.hand) of
          Nothing -> pure Nothing
          Just (Tuple card' hand') -> do
            player' <- drawCards card'.cards player { hand = hand' }
            let atPlay' = card' : player'.atPlay
            pure if not isAction card'
              then Nothing
              else
                let actions' = player'.actions + card'.actions - 1 in
                let buys' = player'.buys + card'.buys in
                Just player' { atPlay = atPlay', actions = actions', buys = buys' }

drawCards :: forall m. MonadEffect m => Int -> Player -> m Player
drawCards n p = if n > 0
  then drawCards (n - 1) =<< drawCard p
  else pure p

drawCard :: forall m. MonadEffect m => Player -> m Player
drawCard player = do
  deck <- if null player.deck
    then do
      liftEffect $ Comm.log $ "Ran out of cards while drawing. Time to shuffle for player: " <> show player
      shuffle player.discard
    else pure player.deck
  let discarded = if null player.deck
    then []
    else player.discard
  pure $ draw (player { deck = deck, discard = discarded })
  where
    draw :: Player -> Player
    draw player =
      let hand' = take 1 player.deck <> player.hand in
      let deck' = drop 1 player.deck in
      player { hand = hand', deck = deck' }

dropIndex :: forall a. Int -> Array a -> Array a
dropIndex i xs =
  let prefix = take i xs in
  let suffix = drop (i + 1) xs in
  prefix <> suffix

newPlayer :: Player
newPlayer =
  { deck: []
  , hand: []
  , discard: (replicate 7 copper) <> (replicate 3 estate)
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
  , specials :: Array Special
  }

card :: Card
card = { types: [], name: "", cost: 0, victoryPoints: 0, treasure: 0, buys: 0, cards: 0, actions: 0, specials: [] }
treasure :: Card
treasure = card { types = [Treasure] }
victory :: Card
victory = card { types = [Victory] }
action :: Card
action = card { types = [Action] }
actionAttack :: Card
actionAttack = card { types = [ Action, Attack ] }
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
curse :: Card
curse = card { types = [Curse], name = "Curse", victoryPoints = -1 }
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
witch :: Card
witch = actionAttack
  { name = "Witch"
  , cost = 5
  , cards = 2
  , specials =
    [ { target: EveryoneElse
      , command: Gain curse
      , description: "Each other player gains a Curse."
      }
    ]
  }
councilRoom :: Card
councilRoom = action
  { name = "Council Room"
  , cost = 5
  , cards = 4
  , buys = 1
  , specials =
    [ { target: EveryoneElse
      , command: Draw 1
      , description: "Each other player draws a card."
      }
    ]
  }

type Special = { target :: Target, command :: Command, description :: String }
data Target = Everyone | EveryoneElse
derive instance genericTarget :: Generic Target _
derive instance eqTarget :: Eq Target
instance showTarget :: Show Target where
  show = genericShow
instance encodeJsonTarget :: EncodeJson Target where
  encodeJson a = genericEncodeJson a
instance decodeJsonTarget :: DecodeJson Target where
  decodeJson a = genericDecodeJson a

data Command = Gain Card | Draw Int
derive instance genericCommand :: Generic Command _
derive instance eqCommand :: Eq Command
instance showCommand :: Show Command where show x = genericShow x
instance encodeJsonCommand :: EncodeJson Command where
  encodeJson a = genericEncodeJson a
instance decodeJsonCommand :: DecodeJson Command where
  decodeJson a = genericDecodeJson a

cleanup :: forall m. MonadEffect m => Player -> m Player
cleanup player = do
  let discard' = player.discard <> player.atPlay <> player.hand <> player.toDiscard <> player.buying
  let player' = player { discard = discard'
    , atPlay = []
    , hand = []
    , toDiscard = []
    , buying = []
    , actions = 1
    , buys = 1
    }
  drawCards 5 player'

shuffle :: forall a m . MonadEffect m => Eq a => Array a -> m (Array a)
shuffle array = fst <$> shuffle' (Tuple [] array)
  where
    shuffle' :: Tuple (Array a) (Array a) -> m (Tuple (Array a) (Array a))
    shuffle' (Tuple shuffled []) = pure $ (Tuple shuffled [])
    shuffle' (Tuple shuffled unshuffled) = do
      i <- liftEffect $ randomInt 0 (length unshuffled - 1)
      let randomElement = take 1 $ drop i unshuffled
      let unshuffledRemainder = dropIndex i unshuffled
      shuffle' (Tuple (randomElement <> shuffled) unshuffledRemainder)

allCards :: Player -> Array Card
allCards player = player.hand <> player.deck <> player.atPlay <> player.discard <> player.toDiscard <> player.buying

score :: Player -> Int
score player = foldr (+) 0 $ map _.victoryPoints (allCards player)

indices :: forall a. Array a -> Array Int
indices xs = fst <$> mapWithIndex Tuple xs
