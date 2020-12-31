module Domination.Data.GameState
  ( GameState(..)
  , Supply
  , Stack
  , newGame
  , play
  , purchase
  , nextPhase
  , setup
  , nextPlayer
  , choiceTurn
  , resolveChoice
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Array
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse, sequence)
import Data.Tuple
import Effect.Class (class MonadEffect)
import Util

import Domination.Data.Card (Card, Target(..), Command(..), SelectCards(..), Special, card, action, actionAttack, treasure, victory, cost, value, isAction, isTreasure, isVictory)
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase (Phase(..), next) as Phase
import Domination.Data.CardType (CardType(..))
import Domination.Data.Choice (Choice(..))

type GameState =
  { turn :: Int
  , phase :: Phase
  , text :: String
  , players :: Array Player
  , supply :: Supply
  }

type Supply = Array Stack
type Stack = { card :: Card, count :: Int }

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
    , { card: chapel, count: 10 }
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
    , { card: scholar, count: 10 }
    ]
  }

nextPhase :: forall m. MonadEffect m => Int -> GameState -> m (Maybe GameState)
nextPhase playerIndex state =
    if playerIndex /= state.turn
    || choicesOutstanding state
    then pure Nothing
    else do
      let phase' = Phase.next state.phase
      let turn' = if state.phase == CleanupPhase
        then nextPlayer state
        else state.turn
      players' <- (if state.phase == CleanupPhase
          then sequence $
            mapWithIndex (\i p -> if i == playerIndex then Player.cleanup p else pure p)
            state.players
          else pure state.players)
      pure $ Just state { phase = phase', turn = turn', players = players' }

setup :: forall m. MonadEffect m => GameState -> m GameState
setup gameState = gameState { players = _ } <$> traverse (Player.drawCards 5) gameState.players

nextPlayer :: GameState -> Int
nextPlayer state = (state.turn + 1) `mod` (length state.players)

purchase :: Int -> Player -> Stack -> GameState -> Maybe GameState
purchase playerIndex player stack state =
  if playerIndex /= state.turn
  || state.phase /= BuyPhase
  || player.buys < 1
  || stack.count < 1
  || Player.cash player < stack.card.cost
  then Nothing
  else
  let player' = player { buying = stack.card : player.buying, buys = player.buys - 1 } in
  let stack' = stack { count = stack.count - 1 } in
  let supply' = (\s -> if s == stack then stack' else s) <$> state.supply in
  state { players = _, supply = supply' } <$> updateAt playerIndex player' state.players

resolveChoice :: Int -> Choice -> GameState -> Maybe GameState
resolveChoice playerIndex (TrashUpTo n Nothing) state = Nothing
resolveChoice playerIndex (TrashUpTo n (Just cardIndices)) state =
  if length cardIndices > n
  then Nothing
  else modifyPlayer playerIndex ((Player.modifyHand $ dropIndices cardIndices) >=> Player.dropChoice) state

play :: forall m. MonadEffect m => Int -> Int -> GameState -> m (Maybe GameState)
play playerIndex cardIndex state =
  if playerIndex /= state.turn
  || choicesOutstanding state
  then pure Nothing
  else case (state.players !! playerIndex) of
  Nothing -> pure Nothing
  Just player' -> case player'.hand !! cardIndex of
    Nothing -> pure Nothing
    Just card -> do
      player'' <- play' player' cardIndex
      case player'' of
        Nothing -> pure Nothing
        Just player''' -> case updateAt playerIndex player''' state.players of
          Nothing -> pure Nothing
          Just players' ->
            let state' = state { players = players' } in
            Just <$> applySpecials state' playerIndex card
    where
    applySpecials :: GameState -> Int -> Card -> m GameState
    applySpecials state playerIndex card =
      foldM (applyEffectToTargets playerIndex) state card.specials

    applyEffectToTargets :: Int -> GameState -> Special -> m GameState
    applyEffectToTargets attackerIndex state { target, command } =
      foldM (\state i -> fromMaybe state <$> applyEffectToTarget command state i) state (targetIndices target attackerIndex state)

    applyEffectToTarget :: Command -> GameState -> Int -> m (Maybe GameState)
    applyEffectToTarget (Gain card) state targetIndex = pure do
      target <- state.players !! targetIndex
      stackIndex <- findIndex (\x -> x.card == card) state.supply
      stack <- state.supply !! stackIndex
      let count' = if stack.count > 0 then stack.count - 1 else stack.count
      let stack' = stack { count = count' }
      supply' <- updateAt stackIndex stack' state.supply
      let target' = if stack.count > 0 then target { discard = stack.card : target.discard } else target
      players' <- updateAt targetIndex target' state.players
      pure (state { players = players', supply = supply' })
    applyEffectToTarget (Draw n) state targetIndex =
      case state.players !! targetIndex of
      Nothing -> pure Nothing
      Just target -> Player.drawCards n target
        <#> \target' -> updateAt targetIndex target' state.players
        <#> state { players = _ }
    applyEffectToTarget (Discard SelectAll) state targetIndex = pure
      $ state.players !! targetIndex
      >>= \target ->
      let toDiscard' = target.toDiscard <> target.hand in
      let target' = target { toDiscard = toDiscard', hand = [] } in
      updateAt targetIndex target' state.players
      <#> state { players = _ }
    applyEffectToTarget (Choose choice) state targetIndex = pure
      $ modifyPlayer targetIndex (Player.gainChoice choice >>> Just) state

    targetIndices :: Target -> Int -> GameState -> Array Int
    targetIndices EveryoneElse attackerIndex = filter (_ /= attackerIndex) <<< indices <<< _.players
    targetIndices Everyone _ = indices <<< _.players
    targetIndices Self attackerIndex = const [ attackerIndex ]

    play' :: Player -> Int -> m (Maybe Player)
    play' player cardIndex =
      case lift2 Tuple (player.hand !! cardIndex) (deleteAt cardIndex player.hand) of
      Nothing -> pure Nothing
      Just (Tuple playedCard hand') -> do
        player' <- Player.drawCards playedCard.cards player { hand = hand' }
        pure if not isAction playedCard
        then Nothing
        else Just player'
          { atPlay = playedCard : player'.atPlay
          , actions = player'.actions + playedCard.actions - 1
          , buys = player'.buys + playedCard.buys
          }

choicesOutstanding :: GameState -> Boolean
choicesOutstanding state = Player.hasChoices `any` state.players

modifyPlayer :: Int -> (Player -> Maybe Player) -> GameState -> Maybe GameState
modifyPlayer playerIndex modify state = do
  player <- state.players !! playerIndex
  player' <- modify player
  players' <- updateAt playerIndex player' state.players
  pure state { players = players' }

-- should we return a maybe here in case there are no players?
-- or should players be a non-empty list?
choiceTurn :: GameState -> Int
choiceTurn state =
  let players' = withIndices state.players in
  let prefix = takeWhile (fst >>> (_ /= state.turn)) players' in
  let suffix = dropWhile (fst >>> (_ /= state.turn)) players' in
  let rotated = suffix <> prefix in
  let withChoices = (snd >>> Player.hasChoices) `filter` rotated in
  fromMaybe 0 $ fst <$> (head withChoices)

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
  , choices: []
  }

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
scholar :: Card
scholar = action
  { name = "Scholar"
  , cost = 5
  , specials =
    [ { target: Self
      , command: Discard SelectAll
      , description: "Discard your hand."
      }
    , { target: Self
      , command: Draw 7
      , description: "Draw 7 cards"
      }
    ]
  }
chapel :: Card
chapel = action
  { name = "Chapel"
  , cost = 2
  , specials =
    [ { target: Self
      , command: Choose $ (TrashUpTo 4 Nothing)
      , description: "Trash up to 4 cards from your hand"
      }
    ]
  }


