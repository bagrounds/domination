module Domination.Data.Game where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (null)
import Data.Array.NonEmpty (NonEmptyArray, replicate, updateAt)
import Data.Foldable (any)
import Data.Lens.Fold ((^?))
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens)
import Data.Lens.Prism (Prism', prism')
import Data.Lens.Record (prop)
import Data.Lens.Setter (set, (%~))
import Data.Lens.Traversal (Traversal', traverseOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Domination.Capability.Random (class Random, randomIntBetween)
import Domination.Data.Card (Card, hasType)
import Domination.Data.Cards as Cards
import Domination.Data.Choice (Choice)
import Domination.Data.Choice as Choice
import Domination.Data.Condition (Condition(..))
import Domination.Data.Phase (Phase(..))
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile
import Domination.Data.Player (Player, newPlayer)
import Domination.Data.Player as Player
import Domination.Data.Result (Result)
import Domination.Data.Stack (Stack, _stacksFromCards)
import Domination.Data.Supply (Supply, getStack, makeSupply)
import Domination.Data.Supply as Supply
import Domination.Data.Wire.Int as Int
import Type.Proxy (Proxy(..))
import Util (assert, fromJust, justIf, (.^))

type Game =
  { phase :: Phase
  , players :: NonEmptyArray Player
  , supply :: Supply
  , trash :: Array Card
  , turn :: Int
  , result :: Maybe Result
  , longGame :: Boolean
  }

_turn
  :: forall a b r
  . Lens { turn :: a | r } { turn :: b | r } a b
_turn = prop (Proxy :: Proxy "turn")

_phase
  :: forall a b r
  . Lens { phase :: a | r } { phase :: b | r } a b
_phase = prop (Proxy :: Proxy "phase")

_players
  :: forall a b r
  . Lens { players :: a | r } { players :: b | r } a b
_players = prop (Proxy :: Proxy "players")

_supply
  :: forall a b r
  . Lens { supply :: a | r } { supply :: b | r } a b
_supply = prop (Proxy :: Proxy "supply")

_trash
  :: forall a b r
  . Lens { trash :: a | r } { trash :: b | r } a b
_trash = prop (Proxy :: Proxy "trash")

_result
  :: forall a b r
  . Lens { result :: a | r } { result :: b | r } a b
_result = prop (Proxy :: Proxy "result")

_player :: Int -> Traversal' Game Player
_player i = _players <<< (ix i)

_stack :: Int -> Traversal' Game Stack
_stack i = _supply <<< Supply._stack i

_ofPhase :: Phase -> Prism' Game Game
_ofPhase phase = prism' identity $ justIf ((==) phase <<< _.phase)

_pile :: Pile -> Int -> Traversal' Game (Array Stack)
_pile pile playerIndex = case pile of
  Pile.AtPlay -> _player playerIndex <<< Player._atPlay <<< _stacksFromCards
  Pile.Buying -> _player playerIndex <<< Player._buying <<< _stacksFromCards
  Pile.Deck -> _player playerIndex <<< Player._deck <<< _stacksFromCards
  Pile.Discard -> _player playerIndex <<< Player._discard <<< _stacksFromCards
  Pile.Discarding -> _player playerIndex <<< Player._toDiscard <<< _stacksFromCards
  Pile.Hand -> _player playerIndex <<< Player._hand <<< _stacksFromCards
  Pile.Supply -> _supply
  Pile.Trash -> _trash <<< _stacksFromCards

new :: Int -> Array Card -> Boolean -> Game
new playerCount cards longGame =
  { turn: zero
  , phase: ActionPhase
  , players: replicate playerCount newPlayer
  , result: Nothing
  , supply: makeSupply playerCount cards
  , trash: []
  , longGame
  }

getPlayer
  :: forall m
  . MonadError String m
  => Int
  -> Game
  -> m Player
getPlayer i = fromJust "cannot get player!" <<< (_ ^? _player i)

updatePlayer
  :: forall m
  . MonadError String m
  => Int
  -> Player
  -> Game
  -> m Game
updatePlayer i player state = fromJust "cannot update player!"
  $ updateAt i player state.players <#> flip (set _players) state

updateStack
  :: forall m
  . MonadError String m
  => Int
  -> Stack
  -> Game
  -> m Game
updateStack i = traverseOf _supply <<< Supply.updateStack i

modifyStack
  :: forall m
  . MonadError String m
  => Int
  -> (Stack -> Stack)
  -> Game
  -> m Game
modifyStack i f state =
  getStack i state.supply <#> f >>= flip (updateStack i) state

modifyPlayer
  :: forall m
  . MonadError String m
  => Int
  -> (Player -> Player)
  -> Game
  -> m Game
modifyPlayer i f state =
  getPlayer i state <#> f >>= flip (updatePlayer i) state

modifyPlayerM
  :: forall m
  . MonadError String m
  => Int
  -> (Player -> m Player)
  -> Game
  -> m Game
modifyPlayerM i f state =
  getPlayer i state >>= f >>= flip (updatePlayer i) state

modifyStackM
  :: forall m
  . MonadError String m
  => Int
  -> (Stack -> m Stack)
  -> Game
  -> m Game
modifyStackM i f state =
  getStack i state.supply >>= f >>= flip (updateStack i) state

upgrade :: Game -> Game
upgrade = (_supply %~ Supply.upgrade)
  >>> (_players %~ map Player.upgrade)
  >>> (_trash %~ map Cards.upgrade)

currentPlayer
  :: forall m
  . MonadError String m
  => Game
  -> m Player
currentPlayer state = getPlayer state.turn state

hasReaction :: Int -> Game -> Boolean
hasReaction playerIndex state =
  case state ^? _player playerIndex <<< Player._reaction of
    Just _ -> true
    Nothing -> false

isAttacked :: Int -> Game -> Boolean
isAttacked playerIndex state =
  fromMaybe false (Choice.isAttack <$> firstChoice playerIndex state)

isBenefitted :: Int -> Game -> Boolean
isBenefitted playerIndex state =
  fromMaybe false (not <<< Choice.isAttack <$> firstChoice playerIndex state)

firstChoice :: Int -> Game -> Maybe Choice
firstChoice playerIndex state =
  state ^? _player playerIndex >>= Player.firstChoice

choicesOutstanding :: Game -> Boolean
choicesOutstanding = view _players >>> any Player.hasChoices

assertTurn
  :: forall m
  . MonadError String m
  => Int
  -> Game
  -> m Game
assertTurn playerIndex = assert
  ((playerIndex == _) <<< _.turn)
  "not your turn!"

assertPhase
  :: forall m
  . MonadError String m
  => Phase
  -> Game
  -> m Game
assertPhase expected = assert
  (_.phase >>> (_ == expected))
  ("Expected: " <> show expected)

assertPlayerCanAfford
  :: forall m
  . MonadError String m
  => Int
  -> Int
  -> Game
  -> m Game
assertPlayerCanAfford playerIndex stackIndex state = do
  stack <- getStack stackIndex state.supply
  modifyPlayerM
    playerIndex
    (Player.assertHasCash stack.card.cost)
    state

assertChoicesResolved
  :: forall m
  . MonadError String m
  => Game
  -> m Game
assertChoicesResolved = assert
  (not <<< choicesOutstanding)
  "error: play: choices outstanding!"

describes :: forall m. Random m => Game -> Condition -> Player -> m Boolean
describes game = case _ of
  HasCard name -> pure <<< _.hand >>> any (_.name >>> (_ == name))
  HasCardType cardType -> pure <<< _.hand >>> any (hasType cardType)
  HasDiscard -> pure <<< _.discard >>> (not <<< null)
  DiscardContains name -> pure <<< _.discard >>> any (_.name >>> (_ == name))
  DiscardContainsCardType cardType -> pure <<< _.discard >>> any (hasType cardType)
  TrashContainsCardType cardType -> pure <<< const (_.trash game) >>> any (hasType cardType)
  Randomly percent -> const
    $ (_ > (percent .^ Int._toWire))
    <$> randomIntBetween zero 100

