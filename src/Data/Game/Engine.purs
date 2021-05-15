module Domination.Data.Game.Engine
  ( makeAutoPlay
  , choiceTurn
  , setup
  ) where

import Prelude hiding (Ordering(..))
import Prim hiding (Constraint)

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array (elem, filter, foldr, head)
import Data.Array as Array
import Data.Array.NonEmpty (mapWithIndex, span, toArray)
import Data.Either (Either(..))
import Data.Foldable (foldM, length, maximum)
import Data.Lens.Setter (over, set)
import Data.Lens.Traversal (traverseOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Domination.Capability.Log (class Log)
import Domination.Capability.Random (class Random)
import Domination.Data.Card (Card, Command(..), Special)
import Domination.Data.Card as Card
import Domination.Data.Cards as Cards
import Domination.Data.Game (Game, assertChoicesResolved, assertPhase, assertPlayerCanAfford, assertTurn, currentPlayer, getPlayer, modifyPlayer, modifyPlayerM, modifyStack, modifyStackM)
import Domination.Data.Game as Game
import Domination.Data.Game.Mode (Mode(..), mode)
import Domination.Data.Game.ResolveChoice (resolveChoice)
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase as Phase
import Domination.Data.Play (Play(..))
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.Result (Result(..))
import Domination.Data.Stack as Stack
import Domination.Data.Supply (emptyStackCount, getStack, highestVictoryCardStackIsEmpty, negativePoints, nonEmptyStacks, positivePoints)
import Domination.Data.Target (Target(..))
import Util (indices, withIndices, (:~))

makeAutoPlay
  :: forall m
  . Random m
  => Log m
  => Play
  -> Game
  -> m (Either String Game)
makeAutoPlay play' state = runExceptT $ do
  state' <- makePlay play' state
  eNextState <- runExceptT $ autoAdvance state'
  pure case eNextState of
    Left _ -> state'
    Right nextState -> nextState

makePlay
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Play
  -> Game
  -> m Game
makePlay play' = maybeGameOver <=< handlePlay play'
  where
    handlePlay :: Play -> Game -> m Game
    handlePlay = case _ of
      EndPhase { playerIndex } -> nextPhase playerIndex
      PlayCard x -> play x
      Purchase x -> purchase x
      ResolveChoice x -> resolveChoice x
      React x -> react x
      DoneReacting _ -> pure

    maybeGameOver :: Game -> m Game
    maybeGameOver state =
      set Game._result <$> finalResult state <*> pure state

autoAdvance
  :: forall m
  . MonadError String m
  => Random m
  => Game
  -> m Game
autoAdvance game = do
  player <- currentPlayer game
  case game.phase of
    ActionPhase ->
      if Player.hasActions player
      && Player.hasActionCardsInHand player
      then pure game
      else advancePhase >>= autoAdvance
    BuyPhase ->
      if player.buys > zero
      && canAffordSomething player
      then pure game
      else advancePhase >>= autoAdvance
    CleanupPhase ->
      advancePhase >>= autoAdvance
  where
    canAffordSomething player = let
      costs = _.card.cost <$> nonEmptyStacks game.supply
      in case Array.uncons costs of
        Nothing -> false
        Just { head, tail } -> let minCost = foldr min head tail in
          minCost <= Player.cash player

    advancePhase :: m Game
    advancePhase = nextPhase game.turn game

nextPhase
  :: forall m
  . MonadError String m
  => Random m
  => Int
  -> Game
  -> m Game
nextPhase playerIndex state =
  assertTurn playerIndex state
    >>= assertChoicesResolved
    >>= modifyPlayerM playerIndex playerUpdate
    <$> nextPlayer >>> over Game._phase Phase.next
  where
    playerUpdate = case state.phase of
      CleanupPhase -> Player.cleanup
      _ -> pure
    nextPlayer s = s
      { turn =
        if s.phase == CleanupPhase
        then (s.turn + one) `mod` (length s.players)
        else s.turn
      }

setup
  :: forall m
  . Random m
  => Game -> m Game
setup game = flip (set Game._players) game
  <$> traverse (Player.drawCards 5) game.players

purchase
  :: forall m
  . MonadError String m
  => { playerIndex :: Int, stackIndex :: Int }
  -> Game
  -> m Game
purchase { playerIndex, stackIndex } =
  assertTurn playerIndex
    >=> assertPhase BuyPhase
    >=> modifyPlayerM playerIndex Player.assertHasBuys
    >=> modifyStackM stackIndex Stack.assertNotEmpty
    >=> assertPlayerCanAfford playerIndex stackIndex
    >=> purchase'
  where
    purchase' :: Game -> m Game
    purchase' state = do
      stack <- getStack stackIndex state.supply
      modifyPlayer playerIndex (Player.purchase stack.card) state
        >>= modifyStack stackIndex Stack.take

react
  :: forall m
  . MonadError String m
  => Log m
  => { playerIndex :: Int, reaction :: Maybe Reaction }
  -> Game
  -> m Game
react { playerIndex, reaction: maybeReaction } =
  case maybeReaction of
    Nothing -> pure
    Just reaction -> case reaction of
      BlockAttack ->
        traverseOf (Game._player playerIndex) Player.dropChoice
      ReactWithChoice choice ->
        modifyPlayer playerIndex (Player._choices :~ choice)

play
  :: forall m
  . MonadError String m
  => Random m
  => { playerIndex :: Int, cardIndex :: Int }
  -> Game
  -> m Game
play { playerIndex, cardIndex } state = do
  player <- getPlayer playerIndex state
  card <- Player.getCard cardIndex player
  assertTurn playerIndex state
    >>= assertChoicesResolved
    >>= modifyPlayerM playerIndex Player.assertHasActions
    >>= modifyPlayerM playerIndex (Player.play cardIndex)
    >>= applySpecialsToTargets card
  where
    applySpecialsToTargets :: Card -> Game -> m Game
    applySpecialsToTargets card =
      flip (foldM $ applySpecialToTargets playerIndex) card.special

    applySpecialToTargets
      :: Int
      -> Game
      -> Special
      -> m Game
    applySpecialToTargets attackerIndex state' { target, command } =
      foldM (flip $ applySpecialToTarget command) state'
      $ targetIndices target attackerIndex state'

    applySpecialToTarget (Choose choice) targetIndex state' =
      modifyPlayer targetIndex (Player.gainChoice choice) state'

    targetIndices :: Target -> Int -> Game -> Array Int
    targetIndices EveryoneElse attackerIndex =
      filter (_ /= attackerIndex)
      <<< toArray
      <<< indices
      <<< _.players
    targetIndices Everyone _ = toArray <<< indices <<< _.players
    targetIndices Self attackerIndex = const [ attackerIndex ]

choiceTurn :: Game -> Int
choiceTurn state =
  let
   players' = withIndices state.players
   { init, rest } = span ((_ /= state.turn) <<< fst) players'
   rotated = rest <> init
   withChoices = (Player.hasChoices <<< snd) `filter` rotated
  in
  fromMaybe zero $ fst <$> (head withChoices)

gameResult :: Game -> Maybe Result
gameResult { players } =
  if length players == 1
  then Just $ Victory 0
  else do
    currentBest <- maximum $ Player.score <$> players
    let winners' = winners currentBest
    if length winners' == 1
    then Victory <$> head winners'
    else Just $ Tie $ winners'
  where
    winners currentBest = fst
      <$> filter
      ((_ == currentBest) <<< Player.score <<< snd)
      (toArray $ Tuple `mapWithIndex` players)

finalResult
  :: forall m
  . MonadError String m
  => Game
  -> m (Maybe Result)
finalResult state@{ players, supply } =
  (if _ then gameResult state else Nothing)
    <<< gameIsOver
    <$> mode state
  where
    gameIsOver = case _ of
      Solo _ ->
        positivePoints supply <= zero
      Short _ _ ->
        emptyStackCount supply >= 3
          || Stack.new (Card._card Cards.province) zero `elem` supply
          || Stack.new (Card._card Cards.colony) zero `elem` supply
          || highestVictoryCardStackIsEmpty supply
      Long p1 p2 ->
        clearWinner || noRemainingPoints
        where
          clearWinner =
            worstOutcomeForCurrentLeader > bestOutcomeForSecondPlace
          noRemainingPoints =
            remainingPoints == zero && remainingCurses == zero
          worstOutcomeForCurrentLeader = currentBest + remainingCurses
          bestOutcomeForSecondPlace =
            Player.positivePoints p2 + remainingPoints
          remainingPoints = positivePoints supply
          remainingCurses = negativePoints supply
          currentBest = max p1Score
            $ fromMaybe p1Score (maximum $ Player.score <$> players)
          p1Score = Player.score p1

