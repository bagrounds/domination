module Domination.Data.GameState
  ( GameState(..)
  , Supply
  , makeAutoPlay
  , makePlay
  , newGame
  , choiceTurn
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Loops (untilJust)
import Control.Monad.State (class MonadState, get)
import Data.Array (dropWhile, filter, findIndex, head, takeWhile, updateAt)
import Data.Either (Either)
import Data.Foldable (any, foldM, length)
import Data.Lens.Fold (preview)
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens')
import Data.Lens.Prism (Prism', prism')
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set)
import Data.Lens.Traversal (Traversal', traverseOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicate)
import Domination.Data.Card (Card, Command(..), Special)
import Domination.Data.Card as Card
import Domination.Data.CardType (CardType(..))
import Domination.Data.Choice (Choice(..))
import Domination.Data.Phase (Phase(..))
import Domination.Data.Phase as Phase
import Domination.Data.Play (Play(..))
import Domination.Data.Player (Player)
import Domination.Data.Player as Player
import Domination.Data.SelectCards (SelectCards(..))
import Domination.Data.Stack (Stack)
import Domination.Data.Stack as Stack
import Domination.Data.Target (Target(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Util (assert, fromJust, indices, justIf, modifyM_, moveAll, prependOver, withIndices)

type GameState =
  { turn :: Int
  , phase :: Phase
  , text :: String
  , players :: Array Player
  , supply :: Supply
  }

_turn :: Lens' GameState Int
_turn = prop (SProxy :: SProxy "turn")
_phase :: Lens' GameState Phase
_phase = prop (SProxy :: SProxy "phase")
_text :: Lens' GameState String
_text = prop (SProxy :: SProxy "text")
_players :: Lens' GameState (Array Player)
_players = prop (SProxy :: SProxy "players")
_supply :: Lens' GameState Supply
_supply = prop (SProxy :: SProxy "supply")

_player :: Int -> Traversal' GameState Player
_player i = _players <<< (ix i)
_stack :: Int -> Traversal' GameState Stack
_stack i = _supply <<< (ix i)

_ofPhase :: Phase -> Prism' GameState GameState
_ofPhase phase = prism' identity $ justIf ((==) phase <<< _.phase)

getPlayer :: forall m. MonadError String m => Int -> GameState -> m Player
getPlayer i = fromJust "cannot get player!" <<< preview (_player i)

updatePlayer :: forall m. MonadError String m => Int -> Player -> GameState -> m GameState
updatePlayer i player state = fromJust "cannot update player!"
  $ updateAt i player state.players <#> flip (set _players) state

updateStack :: forall m. MonadError String m => Int -> Stack -> GameState -> m GameState
updateStack i stack state = fromJust "cannot update stack!"
  $ updateAt i stack state.supply <#> flip (set _supply) state

modifyStack :: forall m. MonadError String m => Int -> (Stack -> Stack) -> GameState -> m GameState
modifyStack i f state = getStack i state <#> f >>= flip (updateStack i) state

modifyPlayer :: forall m. MonadError String m => Int -> (Player -> Player) -> GameState -> m GameState
modifyPlayer i f state = getPlayer i state <#> f >>= flip (updatePlayer i) state

modifyPlayerM :: forall m. MonadError String m => Int -> (Player -> m Player) -> GameState -> m GameState
modifyPlayerM i f state = getPlayer i state >>= f >>= flip (updatePlayer i) state

modifyStackM :: forall m. MonadError String m => Int -> (Stack -> m Stack) -> GameState -> m GameState
modifyStackM i f state = getStack i state >>= f >>= flip (updateStack i) state

getStack :: forall m. MonadError String m => Int -> GameState -> m Stack
getStack i = fromJust "cannot get stack!" <<< preview (_stack i)

indexOfStack :: forall m. MonadError String m => Card -> GameState -> m Int
indexOfStack card state = fromJust "card not in supply!"
  $ findIndex (\x -> x.card == card) state.supply

type Supply = Array Stack

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

makeAutoPlay :: forall m. MonadEffect m => MonadState GameState m => Play -> m (Either String Unit)
makeAutoPlay = (makePlay >=> const autoAdvance) >>> runExceptT

makePlay :: forall m. MonadError String m => MonadEffect m => MonadState GameState m => Play -> m Unit
makePlay p = modifyM_ case p of
  NewGame n -> const $ setup (newGame n)
  EndPhase playerIndex -> nextPhase playerIndex
  PlayCard playerIndex card -> play playerIndex card
  Purchase playerIndex stackIndex -> purchase playerIndex stackIndex
  ResolveChoice playerIndex choice -> resolveChoice playerIndex choice

getCurrentPlayer :: forall m. MonadError String m => GameState -> m Player
getCurrentPlayer state = getPlayer state.turn state

autoAdvance :: forall m. MonadError String m => MonadEffect m => MonadState GameState m => m Unit
autoAdvance = untilJust autoAdvance'
  where
  autoAdvance' :: m (Maybe Unit)
  autoAdvance' = do
    gameState <- get
    liftEffect (Console.log $ "autoAdvance? from " <> show gameState.phase)
    player <- getCurrentPlayer gameState
    case gameState.phase of
      ActionPhase ->
        if Player.hasActions player
        && Player.hasActionCardsInHand player
        then pure $ Just unit
        else advancePhase <#> const Nothing
      BuyPhase ->
        if player.buys > 0
        then pure $ Just unit
        else advancePhase <#> const Nothing
      CleanupPhase ->
        advancePhase <#> const Nothing
    where
      advancePhase :: m Unit
      advancePhase = modifyM_ \s -> nextPhase s.turn s

nextPhase
  :: forall m
  . MonadError String m
  => MonadEffect m
  => Int
  -> GameState
  -> m GameState
nextPhase playerIndex state =
  assertTurn playerIndex state
    -- >>= assertChoicesResolved
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

setup :: forall m. MonadError String m => MonadEffect m => GameState -> m GameState
setup gameState = flip (set _players) gameState
  <$> traverse (Player.drawCards 5) gameState.players

assertPhase :: forall m. MonadError String m => Phase -> GameState -> m GameState
assertPhase expected = assert
  ("Expected: " <> show expected)
  (_.phase >>> (_ == expected))

purchase
  :: forall m
  . MonadError String m
  => Int
  -> Int
  -> GameState
  -> m GameState
purchase playerIndex stackIndex =
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

assertPlayerCanAfford :: forall m. MonadError String m => Int -> Int -> GameState -> m GameState
assertPlayerCanAfford playerIndex stackIndex state = do
  stack <- getStack stackIndex state
  modifyPlayerM playerIndex (Player.assertHasCash stack.card.cost) state

overStackM
  :: forall m a
  . MonadError String m
  => (Stack -> m a)
  -> Int
  -> GameState
  -> m a
overStackM assertion stackIndex state =
  assertion =<< getStack stackIndex state

resolveChoice :: forall m. MonadError String m => Int -> Choice -> GameState -> m GameState
resolveChoice playerIndex (TrashUpTo n Nothing) state = throwError "this is an unresolved choice!"
resolveChoice playerIndex (TrashUpTo n (Just cardIndices)) state
  | length cardIndices > n = throwError "cannot trash more indices than cards in hand!"
  | otherwise = fromJust "failed to trash cards!" $
    maybeModifyPlayer playerIndex playerUpdate state
    where
      playerUpdate = Player.dropCards cardIndices >=> Player.dropChoice

assertTurn :: forall m. MonadError String m => Int -> GameState -> m GameState
assertTurn playerIndex = assert "not your turn!" $ (playerIndex == _) <<< _.turn

assertChoicesResolved :: forall m. MonadError String m => GameState -> m GameState
assertChoicesResolved =
  assert "error: play: choices outstanding!" $ not <<< choicesOutstanding

play :: forall m. MonadError String m => MonadEffect m => Int -> Int -> GameState -> m GameState
play playerIndex cardIndex state = do
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
      flip (foldM $ applySpecialToTargets playerIndex) card.specials

    applySpecialToTargets :: Int -> GameState -> Special -> m GameState
    applySpecialToTargets attackerIndex state { target, command } =
      foldM (flip $ applySpecialToTarget command) state $ targetIndices target attackerIndex state

    applySpecialToTarget :: Command -> Int -> GameState -> m GameState
    applySpecialToTarget (Gain card) targetIndex state = do
      stackIndex <- indexOfStack card state
      stack <- getStack stackIndex state
      let stackUpdate = if stack.count > 0
        then over Stack._count (_ - 1)
        else identity
      state' <- modifyStack stackIndex stackUpdate state
      let playerUpdate = if stack.count > 0
        then prependOver Player._discard stack.card
        else identity
      modifyPlayer targetIndex playerUpdate state'

    applySpecialToTarget (Draw n) targetIndex state =
      modifyPlayerM targetIndex (Player.drawCards n) state

    applySpecialToTarget (Discard SelectAll) targetIndex state =
      modifyPlayer targetIndex (moveAll Player._hand Player._toDiscard) state

    applySpecialToTarget (Choose choice) targetIndex state =
      modifyPlayer targetIndex (Player.gainChoice choice) state

    targetIndices :: Target -> Int -> GameState -> Array Int
    targetIndices EveryoneElse attackerIndex = filter (_ /= attackerIndex) <<< indices <<< _.players
    targetIndices Everyone _ = indices <<< _.players
    targetIndices Self attackerIndex = const [ attackerIndex ]

choicesOutstanding :: GameState -> Boolean
choicesOutstanding = any Player.hasChoices <<< view _players

maybeModifyPlayer :: Int -> (Player -> Maybe Player) -> GameState -> Maybe GameState
maybeModifyPlayer i = traverseOf (_player i)

maybeModifyPlayerHand :: Int -> (Array Card -> Maybe (Array Card)) -> GameState -> Maybe GameState
maybeModifyPlayerHand i = traverseOf (Player._hand >>> _player i)

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
copper = Card.treasure { name = "Copper", treasure = 1 }
silver :: Card
silver = Card.treasure { name = "Silver", cost = 3, treasure = 2 }
gold :: Card
gold = Card.treasure { name = "Gold", cost = 6, victoryPoints = 0, treasure = 3 }
platinum :: Card
platinum = Card.treasure { name = "Platinum", cost = 9, treasure = 5 }
estate :: Card
estate = Card.victory { name = "Estate", cost = 2, victoryPoints  = 1 }
duchy :: Card
duchy = Card.victory { name = "Duchy", cost = 5, victoryPoints = 3 }
province :: Card
province = Card.victory { name = "Province", cost = 8, victoryPoints = 6 }
colony :: Card
colony = Card.victory { name = "Colony", cost = 11, victoryPoints = 10 }
curse :: Card
curse = Card.card { types = [Curse], name = "Curse", victoryPoints = -1 }
greatHall :: Card
greatHall = Card.victory { types = [Action, Victory], name = "Great Hall", cost = 3, cards = 1, actions = 1, victoryPoints = 1 }
village :: Card
village = Card.action { name = "Village", cost = 3, cards = 1, actions = 2 }
woodCutter :: Card
woodCutter = Card.action { name = "Wood Cutter", cost = 3, buys = 1, treasure = 2 }
laboratory :: Card
laboratory = Card.action { name = "Laboratory", cost = 5, cards = 2, actions = 1 }
smithy :: Card
smithy = Card.action { name = "Smithy", cost = 4, cards = 3 }
festival :: Card
festival = Card.action { name = "Festival", cost = 5, actions = 2, buys = 1, treasure = 2 }
market :: Card
market = Card.action { name = "Market", cost = 5, actions = 1, cards = 1, buys = 1, treasure = 1 }
harem :: Card
harem = Card.treasure { types = [Treasure, Victory], name = "Harem", cost = 6, treasure = 2, victoryPoints = 2 }
bazaar :: Card
bazaar = Card.action { name = "Bazaar", cost = 5, cards = 1, actions = 2, treasure = 1 }
monument :: Card
monument = Card.action { types = [Action, Victory], name = "Monument", cost = 4, treasure = 2, victoryPoints = 1 }
workersVillage :: Card
workersVillage = Card.action { name = "Worker's Village", cost = 4, cards = 1, actions = 2, buys = 1 }
witch :: Card
witch = Card.actionAttack
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
councilRoom = Card.action
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
scholar = Card.action
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
chapel = Card.action
  { name = "Chapel"
  , cost = 2
  , specials =
    [ { target: Self
      , command: Choose $ (TrashUpTo 4 Nothing)
      , description: "Trash up to 4 cards from your hand"
      }
    ]
  }


