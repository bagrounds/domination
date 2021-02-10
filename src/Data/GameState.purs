module Domination.Data.GameState where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array (all, catMaybes, dropWhile, filter, find, findIndex, head, length, null, takeWhile, updateAt)
import Data.ArrayBuffer.Class (putArrayBuffer)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Foldable (any, foldM)
import Data.Lens.Fold ((^?))
import Data.Lens.Getter (view, (^.))
import Data.Lens.Index (ix)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Lens (Lens')
import Data.Lens.Prism (Prism', prism', review)
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set, (%~), (.~), (<>~))
import Data.Lens.Traversal (Traversal', traverseOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (replicate)
import Domination.Capability.Random (class Random, randomIntBetween)
import Domination.Data.Bonus (Bonus(..))
import Domination.Data.Card (Card, Command(..), Special)
import Domination.Data.Card as Card
import Domination.Data.CardType (CardType(..))
import Domination.Data.Cards as Cards
import Domination.Data.Choice (Choice(..))
import Domination.Data.Choice as Choice
import Domination.Data.Condition (Condition(..))
import Domination.Data.Constraint (Constraint(..))
import Domination.Data.Filter (Filter(..))
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase as Phase
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player, WirePlayer)
import Domination.Data.Player as Player
import Domination.Data.Reaction (Reaction(..))
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack (Stack, WireStack)
import Domination.Data.Stack as Stack
import Domination.Data.Target (Target(..))
import Domination.Data.WireInt (WireInt, _WireInt)
import Relation (Relation(..))
import Rule (check, lengthIs, (!<>), (!>), (<>!), (<@!))
import Util (assert, dropIndices, fromJust, indices, justIf, moveAll, takeIndices, withIndices, (.^), (<$>~))

type GameState =
  { turn :: Int
  , phase :: Phase
  , players :: Array Player
  , supply :: Supply
  , trash :: Array Card
  }

x ab = putArrayBuffer ab 0 (toWire (newGame 1))

type WireGameState = Tuple Phase
  (Tuple (Array WirePlayer)
  (Tuple (Array WireStack)
  (Tuple (Array WireInt) WireInt)))

toWire :: GameState -> WireGameState
toWire = view _toWire

fromWire :: WireGameState -> GameState
fromWire = review _toWire

_toWire :: Iso' GameState WireGameState
_toWire = iso to from where
  to = (prop _turn %~ view _WireInt)
    >>> (prop _players <$>~ view Player._toWire)
    >>> (prop _supply <$>~ view Stack._toWire)
    >>> (prop _trash <$>~ view Cards._toWire)
    >>> toTuple
  from = fromTuple
    >>> (prop _turn %~ review _WireInt)
    >>> (prop _players <$>~ review Player._toWire)
    >>> (prop _supply <$>~ review Stack._toWire)
    >>> (prop _trash <$>~ (review Cards._toWire))
  _players = SProxy :: SProxy "players"
  _supply = SProxy :: SProxy "supply"
  _trash = SProxy :: SProxy "trash"
  _turn = SProxy :: SProxy "turn"
  toTuple { phase, players, supply, trash, turn } =
    Tuple phase $ Tuple players $ Tuple supply $ Tuple trash turn
  fromTuple
    (Tuple phase (Tuple players (Tuple supply (Tuple trash turn)))) =
    { phase, players, supply, trash, turn }

_turn :: Lens' GameState Int
_turn = prop (SProxy :: SProxy "turn")
_phase :: Lens' GameState Phase
_phase = prop (SProxy :: SProxy "phase")
_players :: Lens' GameState (Array Player)
_players = prop (SProxy :: SProxy "players")
_supply :: Lens' GameState Supply
_supply = prop (SProxy :: SProxy "supply")
_trash :: Lens' GameState (Array Card)
_trash = prop (SProxy :: SProxy "trash")

_player :: Int -> Traversal' GameState Player
_player i = _players <<< (ix i)
_stack :: Int -> Traversal' GameState Stack
_stack i = _supply <<< (ix i)

_ofPhase :: Phase -> Prism' GameState GameState
_ofPhase phase = prism' identity $ justIf ((==) phase <<< _.phase)

getPlayer
  :: forall m
  . MonadError String m
  => Int
  -> GameState
  -> m Player
getPlayer i = fromJust "cannot get player!" <<< (_ ^? _player i)

getPlayerHand
  :: forall m
  . MonadError String m
  => Int
  -> GameState
  -> m (Array Card)
getPlayerHand playerIndex = fromJust "cannot get player hand!"
  <<< (_ ^? _player playerIndex <<< Player._hand)

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
updateStack i stack state = fromJust "cannot update stack!"
  $ updateAt i stack state.supply <#> flip (set _supply) state

modifyStack
  :: forall m
  . MonadError String m
  => Int
  -> (Stack -> Stack)
  -> GameState
  -> m GameState
modifyStack i f state =
  getStack i state <#> f >>= flip (updateStack i) state

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
  getStack i state >>= f >>= flip (updateStack i) state

getStack
  :: forall m
  . MonadError String m
  => Int
  -> GameState
  -> m Stack
getStack i = fromJust "cannot get stack!" <<< (_ ^? _stack i)

stackByName
  :: forall m
  . MonadError String m
  => String
  -> GameState
  -> m Stack
stackByName cardName { supply } = fromJust "card not in supply!" $
  find (view (Stack._card <<< Card._name) >>> (_ == cardName)) supply

indexOfStack
  :: forall m
  . MonadError String m
  => Card
  -> GameState
  -> m Int
indexOfStack card = fromJust "card not in supply!"
  <<< findIndex (view Stack._card >>> (_ == card)) <<< _.supply

type Supply = Array Stack

defaultSupply :: Int -> Array { card :: Card, count :: Int }
defaultSupply playerCount = Cards.cardMap <#> \card ->
  { card, count: countOf card }
  where
    countOf card =
      if Card.hasType Victory card
      then victoryCount
      else if Card.hasType Curse card
      then curseCount
      else if Card.hasType Treasure card
      then treasureCount
      else kingdomCount
    curseCount = 10 * (playerCount - 1)
    victoryCount = 4 * playerCount
    kingdomCount = 4 * playerCount
    treasureCount = 10 * playerCount

newGame :: Int -> GameState
newGame playerCount =
  { turn: 0
  , phase: ActionPhase
  , players: replicate playerCount newPlayer
  , supply: defaultSupply playerCount
  , trash: []
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
makePlay = case _ of
  NewGame { playerCount } -> const $ setup (newGame playerCount)
  EndPhase { playerIndex } -> nextPhase playerIndex
  PlayCard x -> play x
  Purchase x -> purchase x
  ResolveChoice x -> resolveChoice x
  React x -> react x

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
  => GameState -> m GameState
autoAdvance gameState = do
    player <- getCurrentPlayer gameState
    case gameState.phase of
      ActionPhase ->
        if Player.hasActions player
        && Player.hasActionCardsInHand player
        then pure gameState
        else advancePhase >>= autoAdvance
      BuyPhase ->
        if player.buys > 0
        then pure gameState
        else advancePhase >>= autoAdvance
      CleanupPhase ->
        advancePhase >>= autoAdvance
    where
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
        then (s.turn + 1) `mod` (length s.players)
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
      stack <- getStack stackIndex state
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
  stack <- getStack stackIndex state
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
  assertion =<< getStack stackIndex state

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

reaction :: Int -> GameState -> Maybe Reaction
reaction playerIndex state = do
  hand <- state ^? _player playerIndex <<< Player._hand
  head $ catMaybes $ _.reaction <$> hand

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

resolveChoice
  :: forall m
  . MonadError String m
  => Random m
  => { playerIndex :: Int, choice :: Choice }
  -> GameState
  -> m GameState
resolveChoice { playerIndex, choice } state =
  case choice of
    MoveFromTo
      { filter
      , n: constraint
      , source
      , destination
      , resolution: Just cardIndices
      } -> do
      let
        _source = _pile source playerIndex
        _destination = _pile destination playerIndex
      sourcePile <- fromJust "failed to get source" $ state ^? _source
      player <- getPlayer playerIndex state
      selected <- takeIndices cardIndices sourcePile
      remaining <- dropIndices cardIndices sourcePile
      let
        forSelected = ("selected cards" <>! _) >>> (selected <@! _)
        forRemaining = ("remaining cards" <>! _) >>> (remaining <@! _)
        forSource = ("source cards" <>! _) >>> (sourcePile <@! _)
      case constraint of
        UpTo n -> check $
          forSelected $ lengthIs LTE (review _WireInt n)
        DownTo n -> check $
          forRemaining (lengthIs EQ $ review _WireInt n)
          ||
          ( forSource (lengthIs LT $ review _WireInt n)
          && forSelected (lengthIs EQ 0)
          )
        Exactly n -> check $
          forSelected (lengthIs EQ $ review _WireInt n)
          ||
          ( forSource (lengthIs LT $ review _WireInt n)
          && forSelected (lengthIs EQ $ length sourcePile)
          )
      case filter of
        Just f -> check $
          forSelected $ all (passFilter f) !> "illegal choice in"
        Nothing -> pure unit
      pure
        $ _source .~ remaining
        $ over _destination (selected <> _)
        $ state

    GainCards { n, cardName, resolution: Just unit } -> do
      stack <- stackByName cardName state
      stackIndex <- indexOfStack stack.card state
      let cardsToGain = min n stack.count
      let newCount = max 0 (stack.count - n)
      let stackUpdate = Stack._count .~ newCount
      let cards = replicate cardsToGain stack.card
      let playerUpdate = Player._discard <>~ cards
      modifyPlayer playerIndex playerUpdate state
        >>= modifyStack stackIndex stackUpdate
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
    Option { choice, resolution: Just agree } ->
      let
        playerUpdate =
          if agree
          then Player.gainChoice choice
          else identity
      in
      modifyPlayer playerIndex playerUpdate state
    If { resolution: Nothing } -> unresolved
    And { resolution: Nothing } -> unresolved
    Or { resolution: Nothing } -> unresolved
    PickN { resolution: Nothing } -> unresolved
    Option { resolution: Nothing } -> unresolved
    MoveFromTo { resolution: Nothing } -> unresolved
    GainCards { resolution: Nothing } -> unresolved
    GainActions { resolution: Nothing } -> unresolved
    GainBuys { resolution: Nothing } -> unresolved
    Discard { resolution: Nothing } -> unresolved
    Draw { resolution: Nothing } -> unresolved
    GainBonus { resolution: Nothing } -> unresolved
  >>= traverseOf (_player playerIndex) Player.dropChoice
  where
    unresolved =
      throwError $ "this is an unresolved choice: " <> show choice

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
    applySpecialToTargets attackerIndex state { target, command } =
      foldM (flip $ applySpecialToTarget command) state
      $ targetIndices target attackerIndex state

    applySpecialToTarget (Choose choice) targetIndex state =
      modifyPlayer targetIndex (Player.gainChoice choice) state

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
  , discard: (replicate 7 Cards.copper) <> (replicate 3 Cards.estate)
  , toDiscard: []
  , atPlay: []
  , buying: []
  , actions: 1
  , buys: 1
  , choices: []
  , reaction : Nothing
  , bonuses : []
  }

describes :: forall m. Random m => Condition -> Player -> m Boolean
describes = case _ of
  HasCard name -> pure <<< _.hand >>> any (_.name >>> (_ == name))
  HasDiscard -> pure <<< _.discard >>> (not <<< null)
  Randomly percent ->
    const $ (_ > (percent .^ _WireInt)) <$> randomIntBetween 0 100

passFilter :: Filter -> Card -> Boolean
passFilter (HasName name) = _.name >>> (_ == name)

