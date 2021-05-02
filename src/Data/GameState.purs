module Domination.Data.GameState where

import Prelude hiding (Ordering(..))

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array (all, dropWhile, elem, filter, foldr, head, length, mapWithIndex, null, tail, takeWhile, uncons, updateAt, (:))
import Data.Either (Either(..))
import Data.Foldable (any, foldM, maximum)
import Data.Lens.Fold ((^?))
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens', Lens)
import Data.Lens.Prism (Prism', prism')
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set, (%~), (.~))
import Data.Lens.Traversal (Traversal', traverseOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (replicate)
import Domination.Capability.Random (class Random, randomIntBetween)
import Domination.Data.Card (Card, Command(..), Special, _cost, hasType)
import Domination.Data.Cards as Cards
import Domination.Data.Choice (Choice(..))
import Domination.Data.Choice as Choice
import Domination.Data.Condition (Condition(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Constraint as Constraint
import Domination.Data.Filter (Filter(..))
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase as Phase
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.Result (Result(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack (Stack)
import Domination.Data.Stack as Stack
import Domination.Data.StackEvaluation (StackExpression(..), StackValue(..))
import Domination.Data.Supply (Supply, emptyStackCount, getStack, highestVictoryCardStackIsEmpty, indexOfStack, makeSupply, negativePoints, nonEmptyStacks, positivePoints, stackByName)
import Domination.Data.Supply as Supply
import Domination.Data.Target (Target(..))
import Domination.Data.Wire.Int as Int
import Relation (Relation(..))
import Rule (check, lengthIs, (!<>), (!>), (<>!), (<@!))
import Util (assert, dropIndices, fromJust, indices, justIf, moveAll, takeIndices, withIndices, (.^))

type GameState =
  { phase :: Phase
  , players :: Array Player
  , supply :: Supply
  , trash :: Array Card
  , turn :: Int
  , result :: Maybe Result
  , longGame :: Boolean
  }

_turn
  :: forall a b r
  . Lens { turn :: a | r } { turn :: b | r } a b
_turn = prop (SProxy :: SProxy "turn")
_phase :: Lens' GameState Phase
_phase = prop (SProxy :: SProxy "phase")
_players
  :: forall a b r
  . Lens { players :: a | r } { players :: b | r } a b
_players = prop (SProxy :: SProxy "players")
_supply
  :: forall a b r
  . Lens { supply :: a | r } { supply :: b | r } a b
_supply = prop (SProxy :: SProxy "supply")
_trash
  :: forall a b r
  . Lens { trash :: a | r } { trash :: b | r } a b
_trash = prop (SProxy :: SProxy "trash")
_result
  :: forall a b r
  . Lens { result :: a | r } { result :: b | r } a b
_result = prop (SProxy :: SProxy "result")

_player :: Int -> Traversal' GameState Player
_player i = _players <<< (ix i)
_stack :: Int -> Traversal' GameState Stack
_stack i = _supply <<< Supply._stack i

_ofPhase :: Phase -> Prism' GameState GameState
_ofPhase phase = prism' identity $ justIf ((==) phase <<< _.phase)

getPlayer
  :: forall m
  . MonadError String m
  => Int
  -> GameState
  -> m Player
getPlayer i = fromJust "cannot get player!" <<< (_ ^? _player i)

updatePlayer
  :: forall m
  . MonadError String m
  => Int
  -> Player
  -> GameState
  -> m GameState
updatePlayer i player state = fromJust "cannot update player!"
  $ updateAt i player state.players <#> flip (set _players) state

updateStack
  :: forall m
  . MonadError String m
  => Int
  -> Stack
  -> GameState
  -> m GameState
updateStack i = traverseOf _supply <<< Supply.updateStack i

modifyStack
  :: forall m
  . MonadError String m
  => Int
  -> (Stack -> Stack)
  -> GameState
  -> m GameState
modifyStack i f state =
  getStack i state.supply <#> f >>= flip (updateStack i) state

modifyPlayer
  :: forall m
  . MonadError String m
  => Int
  -> (Player -> Player)
  -> GameState
  -> m GameState
modifyPlayer i f state =
  getPlayer i state <#> f >>= flip (updatePlayer i) state

modifyPlayerM
  :: forall m
  . MonadError String m
  => Int
  -> (Player -> m Player)
  -> GameState
  -> m GameState
modifyPlayerM i f state =
  getPlayer i state >>= f >>= flip (updatePlayer i) state

modifyStackM
  :: forall m
  . MonadError String m
  => Int
  -> (Stack -> m Stack)
  -> GameState
  -> m GameState
modifyStackM i f state =
  getStack i state.supply >>= f >>= flip (updateStack i) state

newGame :: Int -> Array Card -> Boolean -> GameState
newGame playerCount cards longGame =
  { turn: zero
  , phase: ActionPhase
  , players: replicate playerCount newPlayer
  , result: Nothing
  , supply: makeSupply playerCount cards
  , trash: []
  , longGame
  }

makeAutoPlay
  :: forall m
  . Random m
  => Play
  -> GameState
  -> m (Either String GameState)
makeAutoPlay p s = runExceptT $ do
  state <- makePlay p s
  eNextState <- runExceptT $ autoAdvance state
  pure case eNextState of
    Left e -> state
    Right nextState -> nextState

makePlay
  :: forall m
  . MonadError String m
  => Random m
  => Play
  -> GameState
  -> m GameState
makePlay play' = maybeGameOver <=< case play' of
  NewGame { playerCount, supply, longGame } ->
    const (setup $ newGame playerCount supply longGame)
  EndPhase { playerIndex } -> nextPhase playerIndex
  PlayCard x -> play x
  Purchase x -> purchase x
  ResolveChoice x -> resolveChoice x
  React x -> react x
  where
    maybeGameOver :: GameState -> m GameState
    maybeGameOver state =
      set _result <$> finalResult state <*> pure state

getCurrentPlayer
  :: forall m
  . MonadError String m
  => GameState
  -> m Player
getCurrentPlayer state = getPlayer state.turn state

autoAdvance
  :: forall m
  . MonadError String m
  => Random m
  => GameState
  -> m GameState
autoAdvance gameState = do
    player <- getCurrentPlayer gameState
    case gameState.phase of
      ActionPhase ->
        if Player.hasActions player
        && Player.hasActionCardsInHand player
        then pure gameState
        else advancePhase >>= autoAdvance
      BuyPhase ->
        if player.buys > zero
        && canAffordSomething player
        then pure gameState
        else advancePhase >>= autoAdvance
      CleanupPhase ->
        advancePhase >>= autoAdvance
    where
      canAffordSomething player = let
        costs = _.card.cost <$> nonEmptyStacks gameState.supply
        in case uncons costs of
          Nothing -> false
          Just { head, tail } -> let minCost = foldr min head tail in
            minCost <= Player.cash player

      advancePhase :: m GameState
      advancePhase = nextPhase gameState.turn gameState

nextPhase
  :: forall m
  . MonadError String m
  => Random m
  => Int
  -> GameState
  -> m GameState
nextPhase playerIndex state =
  assertTurn playerIndex state
    >>= assertChoicesResolved
    >>= modifyPlayerM playerIndex playerUpdate
    <$> nextPlayer >>> over _phase Phase.next
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
  => GameState -> m GameState
setup gameState = flip (set _players) gameState
  <$> traverse (Player.drawCards 5) gameState.players

assertPhase
  :: forall m
  . MonadError String m
  => Phase
  -> GameState
  -> m GameState
assertPhase expected = assert
  (_.phase >>> (_ == expected))
  ("Expected: " <> show expected)

purchase
  :: forall m
  . MonadError String m
  => { playerIndex :: Int, stackIndex :: Int }
  -> GameState
  -> m GameState
purchase { playerIndex, stackIndex } =
  assertTurn playerIndex
    >=> assertPhase BuyPhase
    >=> modifyPlayerM playerIndex Player.assertHasBuys
    >=> modifyStackM stackIndex Stack.assertNotEmpty
    >=> assertPlayerCanAfford playerIndex stackIndex
    >=> purchase'
  where
    purchase' :: GameState -> m GameState
    purchase' state = do
      stack <- getStack stackIndex state.supply
      modifyPlayer playerIndex (Player.purchase stack.card) state
        >>= modifyStack stackIndex Stack.take

assertPlayerCanAfford
  :: forall m
  . MonadError String m
  => Int
  -> Int
  -> GameState
  -> m GameState
assertPlayerCanAfford playerIndex stackIndex state = do
  stack <- getStack stackIndex state.supply
  modifyPlayerM
    playerIndex
    (Player.assertHasCash stack.card.cost)
    state

overStackM
  :: forall m a
  . MonadError String m
  => (Stack -> m a)
  -> Int
  -> GameState
  -> m a
overStackM assertion stackIndex state =
  assertion =<< getStack stackIndex state.supply

hasReaction :: Int -> GameState -> Boolean
hasReaction playerIndex state =
  case state ^? _player playerIndex <<< Player._reaction of
    Just _ -> true
    Nothing -> false

isAttacked :: Int -> GameState -> Boolean
isAttacked playerIndex state =
  case firstChoice playerIndex state of
    Just choice -> Choice.isAttack choice
    _ -> false

firstChoice :: Int -> GameState -> Maybe Choice
firstChoice playerIndex state =
  state ^? _player playerIndex >>= Player.firstChoice

react
  :: forall m
  . MonadError String m
  => { playerIndex :: Int, reaction :: Maybe Reaction }
  -> GameState
  -> m GameState
react { playerIndex, reaction } =
  modifyPlayer playerIndex Player.dropReaction >=>
  case reaction of
    Nothing ->
      pure
    Just BlockAttack ->
      traverseOf (_player playerIndex) Player.dropChoice

_pile :: Pile -> Int -> Traversal' GameState (Array Card)
_pile pile playerIndex = case pile of
  Pile.Hand -> _player playerIndex <<< Player._hand
  Pile.Trash -> _trash
  Pile.Deck -> _player playerIndex <<< Player._deck
  Pile.Discard -> _player playerIndex <<< Player._discard
  Pile.ToDiscard -> _player playerIndex <<< Player._toDiscard

resolveChoice
  :: forall m
  . MonadError String m
  => Random m
  => { playerIndex :: Int, choice :: Choice }
  -> GameState
  -> m GameState
resolveChoice { playerIndex, choice } state =
  case choice of
    StackChoice { attack, expression: expr, stack: s, description } ->
      go expr s state
      where
        go
          :: Array StackExpression
          -> Array StackValue
          -> GameState
          -> m GameState
        go expression stack state' =
          case uncons expression of
            Nothing ->
              case head stack of
                Nothing -> pure state'
                Just x -> throwError $
                  "Empty expression but non-empty stack: "
                  <> show stack
            Just { head: e, tail: expressionTail } -> case e of
              StackChooseCardsFromHand constraint (Just v) ->
                go expressionTail (StackArrayInt v : stack) state'
              StackChooseCardsFromHand constraint Nothing -> let
                choice' = StackChoice
                  { attack
                  , expression
                  , stack
                  , description
                  }
                in
                traverseOf
                  (_player playerIndex)
                  Player.dropChoice
                  state'
                  -- TODO: clean up this hack
                  -- HACK: adding same choice twice because we drop
                  -- a choice at the end of resolveChoice
                  -- unconditionally.
                  >>= modifyPlayer
                    playerIndex
                    (Player.gainChoices [choice', choice'])
              StackDuplicate ->
                case head stack of
                  Nothing -> throwError
                    "StackDuplicate with empty Stack"
                  Just v ->
                    go expressionTail (v : stack) state'
              StackDiscard ->
                case head stack, tail stack of
                  Nothing, _ -> throwError
                    "StackDuplicate with empty Stack"
                  Just (StackArrayInt cardIndices)
                    , (Just stackTail) -> do
                    state'' <- moveFromTo playerIndex state'
                      { filter: Nothing
                      , n: Exactly $ length cardIndices
                      , source: Pile.Hand
                      , destination: Pile.ToDiscard
                      , resolution: Just cardIndices
                      , attack
                      }
                    go expressionTail stackTail state''
                  Just x, _ -> throwError $
                    "can't discard " <> show x
              StackLength ->
                case head stack, tail stack of
                  Nothing, _ -> throwError
                    "StackDuplicate with empty Stack"
                  Just (StackArrayInt ints), Just stackTail -> let
                    stack' = StackInt (length ints) : stackTail
                    in go expressionTail stack' state'
                  Just x, _ -> throwError $
                    "can't take the length of " <> show x
              StackDraw ->
                case head stack, tail stack of
                  Nothing, _ -> throwError
                    "StackDuplicate with empty Stack"
                  Just (StackInt n), Just stackTail ->
                    -- TODO: deduplicate code
                    modifyPlayerM
                      playerIndex
                      (Player.drawCards n)
                      state'
                      >>= go expressionTail stackTail
                  Just x, _ -> throwError $
                    "can't draw " <> show x

    MoveFromTo body -> moveFromTo playerIndex state body

    GainCard { filter, destination, resolution: Just cardName } -> do
      let _destination = _pile destination playerIndex
      stack <- stackByName cardName state.supply
      stackIndex <- indexOfStack stack.card state.supply
      let
        newCount = stack.count - one
        cards = [ stack.card ]
      over _destination (cards <> _)
        <$> modifyStack stackIndex Stack.take state

    GainCards { n, cardName, destination, resolution: Just unit } -> do
      let _destination = _pile destination playerIndex
      { card, count } <- stackByName cardName state.supply
      stackIndex <- indexOfStack card state.supply
      let
        cardsToGain = min n count
        newCount = max zero (count - n)
        stackUpdate = Stack._count .~ newCount
        cards = replicate cardsToGain card
      over _destination (cards <> _)
        <$> modifyStack stackIndex stackUpdate state
    GainActions { n, resolution: Just unit } ->
      modifyPlayer playerIndex (Player.gainActions n) state
    GainBuys { n, resolution: Just unit } ->
      modifyPlayer playerIndex (Player.gainBuys n) state
    Discard { selection: SelectAll, resolution: Just unit } ->
      modifyPlayer playerIndex (moveAll Player._hand Player._toDiscard) state
    Draw { n, resolution: Just unit } ->
      modifyPlayerM playerIndex (Player.drawCards n) state
    GainBonus { bonus, resolution: Just unit } ->
      modifyPlayer playerIndex (Player.gainBonus bonus) state
    If { choice: choice', otherwise, condition, resolution: Just unit } -> do
      player <- getPlayer playerIndex state
      ok <- condition `describes` player
      modifyPlayer playerIndex (playerUpdate ok) state
      where
        playerUpdate ok player = do
          if ok
          then Player.gainChoice choice' player
          else case otherwise of
            Nothing -> player
            Just c -> Player.gainChoice c player
    And { choices, resolution: Just unit } ->
      modifyPlayer playerIndex (Player.gainChoices choices) state
    Or { resolution: Just chosen } ->
      modifyPlayer playerIndex (Player.gainChoice chosen) state
    PickN { n, resolution: Just choices } -> do
      check $ choices <@! lengthIs EQ n !<> "choices"
      modifyPlayer playerIndex (Player.gainChoices choices) state
    Option { choice: choice', resolution: Just agree } ->
      let
        playerUpdate =
          if agree
          then Player.gainChoice choice'
          else identity
      in
      modifyPlayer playerIndex playerUpdate state
    If { resolution: Nothing } -> unresolved
    And { resolution: Nothing } -> unresolved
    Or { resolution: Nothing } -> unresolved
    PickN { resolution: Nothing } -> unresolved
    Option { resolution: Nothing } -> unresolved
    GainCards { resolution: Nothing } -> unresolved
    GainCard { filter: cardFilter, resolution: Nothing } -> do
      let
        unfiltered :: Array Card
        unfiltered = _.card <$> nonEmptyStacks state.supply
        cards :: Array Card
        cards = case cardFilter of
          Nothing -> unfiltered
          Just f -> passFilter f `filter` unfiltered
      case cards of
        [] -> pure state
        xs -> unresolved
    GainActions { resolution: Nothing } -> unresolved
    GainBuys { resolution: Nothing } -> unresolved
    Discard { resolution: Nothing } -> unresolved
    Draw { resolution: Nothing } -> unresolved
    GainBonus { resolution: Nothing } -> unresolved
  >>= traverseOf (_player playerIndex) Player.dropChoice
  where
    unresolved =
      throwError $ "this is an unresolved choice: " <> show choice

type MoveFromToRecord =
  { filter :: Maybe Filter
  , n :: Constraint
  , source :: Pile
  , destination :: Pile
  , resolution :: Maybe (Array Int)
  , attack :: Boolean
  }

moveFromTo
  :: forall m
  . MonadError String m
  => Random m
  => Int
  -> GameState
  -> MoveFromToRecord
  -> m GameState
moveFromTo _ _ choice@{ resolution: Nothing } =
  throwError $ "this is an unresolved choice: " <> show choice
moveFromTo playerIndex state
  { filter
  , n: constraint
  , source
  , destination
  , resolution: Just cardIndices
  } = do
  let
    _source = _pile source playerIndex
    _destination = _pile destination playerIndex
  sourcePile <- fromJust "failed to get source" $ state ^? _source
  selected <- takeIndices cardIndices sourcePile
  remaining <- dropIndices cardIndices sourcePile
  Constraint.check constraint selected remaining sourcePile
  case filter of
    Just f ->
      let  forSelected = ("selected cards" <>! _) >>> (selected <@! _)
      in check
        $ forSelected
        $ all (passFilter f) !> "illegal choice in"
    Nothing -> pure unit
  pure
    $ _source .~ remaining
    $ over _destination (selected <> _)
    $ state


assertTurn
  :: forall m
  . MonadError String m
  => Int
  -> GameState
  -> m GameState
assertTurn playerIndex = assert
  ((playerIndex == _) <<< _.turn)
  "not your turn!"

assertChoicesResolved
  :: forall m
  . MonadError String m
  => GameState
  -> m GameState
assertChoicesResolved = assert
  (not <<< choicesOutstanding)
  "error: play: choices outstanding!"

play
  :: forall m
  . MonadError String m
  => Random m
  => { playerIndex :: Int, cardIndex :: Int }
  -> GameState
  -> m GameState
play { playerIndex, cardIndex } state = do
  player <- getPlayer playerIndex state
  card <- Player.getCard cardIndex player
  assertTurn playerIndex state
    >>= assertChoicesResolved
    >>= modifyPlayerM playerIndex Player.assertHasActions
    >>= modifyPlayerM playerIndex (Player.play cardIndex)
    >>= applySpecialsToTargets card
  where
    applySpecialsToTargets :: Card -> GameState -> m GameState
    applySpecialsToTargets card =
      flip (foldM $ applySpecialToTargets playerIndex) card.special

    applySpecialToTargets
      :: Int
      -> GameState
      -> Special
      -> m GameState
    applySpecialToTargets attackerIndex state' { target, command } =
      foldM (flip $ applySpecialToTarget command) state'
      $ targetIndices target attackerIndex state'

    applySpecialToTarget (Choose choice) targetIndex state' =
      modifyPlayer targetIndex (Player.gainChoice choice) state'

    targetIndices :: Target -> Int -> GameState -> Array Int
    targetIndices EveryoneElse attackerIndex =
      filter (_ /= attackerIndex) <<< indices <<< _.players
    targetIndices Everyone _ = indices <<< _.players
    targetIndices Self attackerIndex = const [ attackerIndex ]

choicesOutstanding :: GameState -> Boolean
choicesOutstanding = view _players >>> any Player.hasChoices

-- should we return a maybe here in case there are no players?
-- or should players be a non-empty list?
choiceTurn :: GameState -> Int
choiceTurn state =
  let
   players' = withIndices state.players
   prefix = takeWhile (fst >>> (_ /= state.turn)) players'
   suffix = dropWhile (fst >>> (_ /= state.turn)) players'
   rotated = suffix <> prefix
   withChoices = (snd >>> Player.hasChoices) `filter` rotated
  in
  fromMaybe zero $ fst <$> (head withChoices)

newPlayer :: Player
newPlayer =
  { deck: []
  , hand: []
  , discard: (replicate 7 Cards.copper) <> (replicate 3 Cards.estate)
  , toDiscard: []
  , atPlay: []
  , buying: []
  , actions: one
  , buys: one
  , choices: []
  , reaction : Nothing
  , bonuses : []
  }

describes :: forall m. Random m => Condition -> Player -> m Boolean
describes = case _ of
  HasCard name -> pure <<< _.hand >>> any (_.name >>> (_ == name))
  HasCardType cardType -> pure <<< _.hand >>> any (hasType cardType)
  HasDiscard -> pure <<< _.discard >>> (not <<< null)
  Randomly percent ->
    const $ (_ > (percent .^ Int._toWire)) <$> randomIntBetween zero 100

passFilter :: Filter -> Card -> Boolean
passFilter = case _ of
  HasName name -> _.name >>> (_ == name)
  HasType cardType -> hasType cardType
  CostUpTo cost -> (_ <= cost) <<< (view _cost)

upgrade :: GameState -> GameState
upgrade = (_supply %~ Supply.upgrade)
  >>> (_players %~ map Player.upgrade)
  >>> (_trash %~ map Cards.upgrade)

gameResult :: GameState -> Maybe Result
gameResult { players } =
  if length players == one
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
      (Tuple `mapWithIndex` players)

data GameMode
  = Solo Player
  | Short Player Player
  | Long Player Player

gameMode
  :: forall m
  . MonadError String m
  => GameState
  -> m GameMode
gameMode { players, longGame } =
  case uncons players, longGame of
    Just { head: player, tail: [] }, _ -> pure $ Solo player
    Just { head: p1, tail }, true -> Long p1 <$> p2 tail
    Just { head: p1, tail }, _ -> Short p1 <$> p2 tail
    Nothing, _ -> throwError "There should never be zero players!"
  where
    p2 = head >>> case _ of
      Nothing -> throwError "Where is player 2???"
      Just player2 -> pure player2

finalResult
  :: forall m
  . MonadError String m
  => GameState
  -> m (Maybe Result)
finalResult state@{ players, supply, longGame } =
  (if _ then gameResult state else Nothing)
    <<< gameIsOver
    <$> gameMode state
  where
    gameIsOver = case _ of
      Solo _ ->
        positivePoints supply <= zero
      Short p1 p2 ->
        emptyStackCount supply >= 3
          || Stack.new Cards.province zero `elem` supply
          || Stack.new Cards.colony zero `elem` supply
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

