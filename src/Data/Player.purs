module Domination.Data.Player where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (catMaybes, deleteAt, filter, head, length, (:))
import Data.Foldable (foldr, null)
import Data.Lens.Fold (firstOf, preview)
import Data.Lens.Getter (view, viewOn)
import Data.Lens.Index (ix)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Lens (Lens')
import Data.Lens.Prism (review)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set, (+~), (.~))
import Data.Lens.Traversal (Traversal', traverseOf, traversed)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Domination.Capability.Random (class Random, shuffle)
import Domination.Data.Bonus (Bonus)
import Domination.Data.Bonus as Bonus
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.Cards as Cards
import Domination.Data.Choice (Choice)
import Domination.Data.Choice as Choice
import Domination.Data.Reaction (Reaction)
import Relation (Relation, is)
import Rule (Rule, check, (!>), (<@!))
import Util (assert, decOver, dropIndices, fromJust, moveOne, prependOver, (:~), (<$>~))

type Player =
  { deck :: Array Card
  , hand :: Array Card
  , discard :: Array Card
  , toDiscard :: Array Card
  , atPlay :: Array Card
  , buying :: Array Card
  , actions :: Int
  , buys :: Int
  , choices :: Array Choice
  , reaction :: Maybe Reaction
  , bonuses :: Array Bonus
  }

type WirePlayer =
  (Tuple Int
  (Tuple (Array Int)
  (Tuple (Array Bonus)
  (Tuple (Array Int)
  (Tuple Int
  (Tuple (Array Choice)
  (Tuple (Array Int)
  (Tuple (Array Int)
  (Tuple (Array Int)
  (Tuple (Maybe Reaction) (Array Int)))))))))))

_toWire :: Iso' Player WirePlayer
_toWire = iso to from where
  to = (prop _deck <$>~ view Cards._toWire)
    >>> (prop _hand <$>~ view Cards._toWire)
    >>> (prop _discard <$>~ view Cards._toWire)
    >>> (prop _toDiscard <$>~ view Cards._toWire)
    >>> (prop _atPlay <$>~ view Cards._toWire)
    >>> (prop _buying <$>~ view Cards._toWire)
    >>> toTuple
  from = fromTuple
    >>> (prop _deck <$>~ review Cards._toWire)
    >>> (prop _hand <$>~ review Cards._toWire)
    >>> (prop _discard <$>~ review Cards._toWire)
    >>> (prop _toDiscard <$>~ review Cards._toWire)
    >>> (prop _atPlay <$>~ review Cards._toWire)
    >>> (prop _buying <$>~ review Cards._toWire)
  _deck = SProxy :: SProxy "deck"
  _hand = SProxy :: SProxy "hand"
  _discard = SProxy :: SProxy "discard"
  _toDiscard = SProxy :: SProxy "toDiscard"
  _atPlay = SProxy :: SProxy "atPlay"
  _buying = SProxy :: SProxy "buying"
  toTuple
    { actions
    , atPlay
    , bonuses
    , buying
    , buys
    , choices
    , deck
    , discard
    , hand
    , reaction
    , toDiscard
    } = Tuple actions
      $ Tuple atPlay
      $ Tuple bonuses
      $ Tuple buying
      $ Tuple buys
      $ Tuple choices
      $ Tuple deck
      $ Tuple discard
      $ Tuple hand
      $ Tuple reaction toDiscard
  fromTuple
    (Tuple actions
    (Tuple atPlay
    (Tuple bonuses
    (Tuple buying
    (Tuple buys
    (Tuple choices
    (Tuple deck
    (Tuple discard
    (Tuple hand
    (Tuple reaction toDiscard)))))))))) =
    { actions
    , atPlay
    , bonuses
    , buying
    , buys
    , choices
    , deck
    , discard
    , hand
    , reaction
    , toDiscard
    }

_deck :: Lens' Player (Array Card)
_deck = prop (SProxy :: SProxy "deck")
_hand :: Lens' Player (Array Card)
_hand = prop (SProxy :: SProxy "hand")
_discard :: Lens' Player (Array Card)
_discard = prop (SProxy :: SProxy "discard")
_toDiscard :: Lens' Player (Array Card)
_toDiscard = prop (SProxy :: SProxy "toDiscard")
_atPlay :: Lens' Player (Array Card)
_atPlay = prop (SProxy :: SProxy "atPlay")
_buying :: Lens' Player (Array Card)
_buying = prop (SProxy :: SProxy "buying")
_actions :: Lens' Player Int
_actions = prop (SProxy :: SProxy "actions")
_buys :: Lens' Player Int
_buys = prop (SProxy :: SProxy "buys")
_choices :: Lens' Player (Array Choice)
_choices = prop (SProxy :: SProxy "choices")
_reaction :: Traversal' Player Reaction
_reaction = prop (SProxy :: SProxy "reaction") <<< _Just
_bonuses :: Lens' Player (Array Bonus)
_bonuses = prop (SProxy :: SProxy "bonuses")

_cardInHand :: Int -> Traversal' Player Card
_cardInHand i = _hand <<< ix i

getCard :: forall m. MonadError String m => Int -> Player -> m Card
getCard i = fromJust "cannot get card!" <<< preview (_cardInHand i)

dropCard :: forall m. MonadError String m => Int -> Player -> m (Array Card)
dropCard i = fromJust "cannot drop card!" <<< deleteAt i <<< _.hand

dropCards
  :: forall m
  . MonadError String m
  => Array Int
  -> Player
  -> m Player
dropCards = traverseOf _hand <<< dropIndices

hasChoices :: Player -> Boolean
hasChoices = _.choices >>> not null

play
  :: forall m
  . MonadError String m
  => Random m
  => Int -> Player -> m Player
play cardIndex player = do
  playedCard <- getCard cardIndex player
  hand' <- dropCard cardIndex player
  player' <- drawCards playedCard.cards (_hand .~ hand' $ player)
  check $ playedCard <@! Card.isAction !> "must play action cards"
  pure $ player' # (_atPlay :~ playedCard)
    >>> (_actions +~ (playedCard.actions - 1))
    >>> (_buys +~ playedCard.buys)

firstChoice :: Player -> Maybe Choice
firstChoice = firstOf traversed <<< view _choices

dropChoice
  :: forall m
  . MonadError String m
  => Player
  -> m Player
dropChoice = fromJust "failed to drop choice"
  <<< traverseOf _choices (deleteAt 0)

gainBonus :: Bonus -> Player -> Player
gainBonus = prependOver _bonuses

gainActions :: Int -> Player -> Player
gainActions n = over _actions (_ + n)

gainBuys :: Int -> Player -> Player
gainBuys n = over _buys (_ + n)

gainChoices :: Array Choice -> Player -> Player
gainChoices = flip $ foldr gainChoice

gainChoice :: Choice -> Player -> Player
gainChoice choice player =
  let
    player' = (over _choices $ (_ <> [ choice ])) player
  in
    if Choice.isAttack choice
    then case reactionInHand player of
      Just r -> gainReaction r player'
      Nothing -> player'
    else player'

gainReaction :: Reaction -> Player -> Player
gainReaction reaction = _ { reaction = Just reaction }

dropReaction :: Player -> Player
dropReaction = _ { reaction = Nothing }

reactionInHand :: Player -> Maybe Reaction
reactionInHand =
  _.hand >>> map _.reaction >>> catMaybes >>> head

hasReaction :: Player -> Boolean
hasReaction { reaction } =
  case reaction of
    Just _ -> true
    Nothing -> false

purchase :: Card -> Player -> Player
purchase card = decOver _buys >>> prependOver _buying card

hasActions :: Player -> Boolean
hasActions = (_ > 0) <<< _.actions

hasActionCardsInHand :: Player -> Boolean
hasActionCardsInHand = not null <<< filter Card.isAction <<< _.hand

cash :: Player -> Int
cash player = (Card.value player.atPlay)
  + (Bonus.cashValue player.bonuses)
  + (Card.value $ Card.isTreasure `filter` player.hand)
  - (Card.cost player.buying)

drawCards
  :: forall m
  . MonadError String m
  => Random m
  => Int -> Player -> m Player
drawCards n p = if n > 0
  then drawCard p >>= drawCards (n - 1)
  else pure p

drawCard
  :: forall m
  . MonadError String m
  => Random m
  => Player -> m Player
drawCard player = do
  deck <- if null player.deck
    then do
      shuffle player.discard
    else pure player.deck
  let discarded = if null player.deck
    then []
    else player.discard
  let player' = player { deck = deck, discard = discarded }
  pure $ drawIfPossible player'

drawIfPossible :: Player -> Player
drawIfPossible p = fromMaybe p $ moveOne _deck _hand $ p

cleanup
  :: forall m
  . MonadError String m
  => Random m
  => Player
  -> m Player
cleanup player = drawCards 5 player
  { discard = player.discard
    <> player.atPlay
    <> player.hand
    <> player.toDiscard
    <> player.buying
  , atPlay = []
  , hand = []
  , toDiscard = []
  , buying = []
  , actions = 1
  , buys = 1
  , bonuses = []
  }

allCards :: Player -> Array Card
allCards player
  = player.hand
  <> player.deck
  <> player.atPlay
  <> player.discard
  <> player.toDiscard
  <> player.buying

score :: Player -> Int
score player = foldr (+) 0 $ _.victoryPoints <$> (allCards player)

assertHasBuys :: forall m. MonadError String m => Player -> m Player
assertHasBuys = assert (_.buys >>> (_ > 0)) "no buys!"

assertHasActions :: forall m. MonadError String m => Player -> m Player
assertHasActions = assert (_.actions >>> (_ > 0)) "no actions!"

assertHasCash
  :: forall m
  . MonadError String m
  => Int
  -> Player
  -> m Player
assertHasCash i = assert (cash >>> (_ >= i)) "not enough treasure!"

hasCash :: Int -> Rule Player
hasCash i = cash >>> (_ >= i) !> ("need $" <> show i)

handSizeIs :: Relation -> Int -> Rule Player
handSizeIs r i = _.hand >>> length >>> is r i
  !> "must have " <> show r <> " " <> show i <> " cards in hand"

