--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| This is a Haskell implementation of the game Scythe, written by Jamey Stegmaier. It's a deck-building game with area control and resource management mechanics.
--|
--| Here's a brief overview of what this code does:
--|
--| 1. **Game setup**: The `play` function sets up a new game state, which includes the player index, card index, and the game supply (representing resources).
--| 2. **Player actions**: When a player takes their turn, they can play cards from their hand to perform various actions, such as:
--| 	* Playing cards to gain points or build structures
--| 	* Using special abilities on cards or other players
--| 	* Moving armies to adjacent provinces
--| 3. **Card effects**: Cards have special effects that are applied when played. These effects can include bonuses, penalties, or changes to the game state.
--| 4. **Game state management**: The `modifyPlayer` and `modifyStack` functions update the player's and stack's states accordingly, reflecting changes made by cards or actions taken by players.
--| 5. **Game logic**: The code uses a combination of monads (e.g., `MonadState`) and functions like `finalResult` to manage the game state and enforce rules, such as:
--| 	* Checking for game over conditions
--| 	* Updating scores and victory points
--| 	* Managing the supply of resources
--| 6. **Monadic computations**: Many parts of the code are written in a monadic style, using functions like `modifyPlayer`, `modifyStack`, and `applySpecialToTargets` to perform computations that modify the game state.
--|
--| Some notable aspects of this implementation include:
--|
--| * The use of a `Maybe Result` return type for the `finalResult` function, which represents the outcome of the game.
--| * The separation of concerns between different functions (e.g., `play`, `modifyPlayer`, `applySpecialToTargets`) that focus on specific tasks within the game.
--| * The use of monads to manage game state and enforce rules, making the code more predictable and easier to reason about.
--|
--| If you're interested in learning more about Scythe or deck-building games, I'd be happy to help with any questions!
--|
--| ### Key Concepts
--| This is a Haskell code that implements various game logic for a popular board game. The game appears to be about building and managing colonies on a planet, while also fighting off enemies and trying to win the most points.
--|
--| Here's a high-level overview of what each part of the code does:
--|
--| 1. **Game setup**: The `Game` type is defined with various fields such as `players`, `supply`, and `mode`. The `mode` field determines whether the game is played in Solo or Long mode.
--| 2. **Player actions**: The `play` function allows a player to perform an action, such as playing a card from their hand or choosing a reaction. The `applySpecialsToTargets` function applies special abilities to targets.
--| 3. **Choice turn**: The `choiceTurn` function determines the current turn order based on the players' choices and scores.
--| 4. **Game result**: The `gameResult` function calculates the final score for each player, including any ties or losses.
--| 5. **Final game state**: The `finalResult` function checks if the game is over based on various conditions, such as empty stack count or remaining points.
--|
--| Some specific interesting parts of the code include:
--|
--| * The use of type classes and generic programming to define functions like `applySpecialsToTargets`, which can work with different types of players and targets.
--| * The use of pattern matching to handle different game modes (Solo, Short, Long) and edge cases.
--| * The implementation of various conditions for determining if the game is over, such as empty stack count or remaining points.
--|
--| Overall, this code appears to be well-structured and follows good design principles. However, without more context about the specific requirements and constraints of the game, it's difficult to provide more detailed feedback on its quality or suggest improvements.
module Domination.Data.Game.Engine where

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
import Util (indices, withIndices)

makeAutoPlay
  :: forall m
  . Random m
  => Log m
  => Play
  -> Game
  -> m (Either String Game)
makeAutoPlay p s = runExceptT $ do
  state <- makePlay p s
  eNextState <- runExceptT $ autoAdvance state
  pure case eNextState of
    Left _ -> state
    Right nextState -> nextState

makePlay
  :: forall m
  . MonadError String m
  => Random m
  => Log m
  => Play
  -> Game
  -> m Game
makePlay play' = maybeGameOver <=< case play' of
  NewGame { playerCount, supply, longGame } ->
    const (setup $ Game.new playerCount supply longGame)
  EndPhase { playerIndex } -> nextPhase playerIndex
  PlayCard x -> play x
  Purchase x -> purchase x
  ResolveChoice x -> resolveChoice x
  React x -> react x
  where
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
  . MonadError String m
  => Random m
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
  => { playerIndex :: Int, reaction :: Maybe Reaction }
  -> Game
  -> m Game
react { playerIndex, reaction } =
  modifyPlayer playerIndex Player.dropReaction >=>
  case reaction of
    Nothing ->
      pure
    Just BlockAttack ->
      traverseOf (Game._player playerIndex) Player.dropChoice

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
