module Domination.Data.Player where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (any, catMaybes, deleteAt, filter, length, replicate, reverse)
import Data.Foldable (foldr, null, sum)
import Data.Lens.Fold (firstOf, preview)
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens)
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, (%~), (+~), (.~))
import Data.Lens.Traversal (Traversal', traverseOf, traversed)
import Data.Maybe (Maybe, fromMaybe)
import Domination.Capability.Random (class Random, randomIntBetween, shuffle)
import Domination.Data.Actions (Actions)
import Domination.Data.Bonus (Bonus)
import Domination.Data.Bonus (cashValue) as Bonus
import Domination.Data.Buys (Buys)
import Domination.Data.Card (Card, _card, hasType)
import Domination.Data.Card (cost, isAction, isTreasure, negativePoints, positivePoints, value) as Card
import Domination.Data.CardType as CardType
import Domination.Data.Cards as Cards
import Domination.Data.Choice (Choice)
import Domination.Data.Condition (Condition(..))
import Domination.Data.Points (Points)
import Domination.Data.Reaction (Reaction)
import Domination.Data.Wire.Int as Int
import Relation (Relation, is)
import Rule (Rule, check, (!>), (<@!))
import Type.Proxy (Proxy(..))
import Util (assert, decOver, dropIndices, fromJust, moveOne, prependOver, (.^), (:~))

type Player =
  { actions :: Actions
  , atPlay :: Array Card
  , bonuses :: Array Bonus
  , buying :: Array Card
  , buys :: Buys
  , choices :: Array Choice
  , deck :: Array Card
  , discard :: Array Card
  , hand :: Array Card
  , toDiscard :: Array Card
  }

_deck
  :: forall a b r
  . Lens { deck :: a | r } { deck :: b | r } a b
_deck = prop (Proxy :: Proxy "deck")

_hand
  :: forall a b r
  . Lens { hand :: a | r } { hand :: b | r } a b
_hand = prop (Proxy :: Proxy "hand")

_discard
  :: forall a b r
  . Lens { discard :: a | r } { discard :: b | r } a b
_discard = prop (Proxy :: Proxy "discard")

_toDiscard
  :: forall a b r
  . Lens { toDiscard :: a | r } { toDiscard :: b | r } a b
_toDiscard = prop (Proxy :: Proxy "toDiscard")

_atPlay
  :: forall a b r
  . Lens { atPlay :: a | r } { atPlay :: b | r } a b
_atPlay = prop (Proxy :: Proxy "atPlay")

_buying
  :: forall a b r
  . Lens { buying :: a | r } { buying :: b | r } a b
_buying = prop (Proxy :: Proxy "buying")

_actions
  :: forall a b r
  . Lens { actions :: a | r } { actions :: b | r } a b
_actions = prop (Proxy :: Proxy "actions")

_buys
  :: forall a b r
  . Lens { buys :: a | r } { buys :: b | r } a b
_buys = prop (Proxy :: Proxy "buys")

_choices
  :: forall a b r
  . Lens { choices :: a | r } { choices :: b | r } a b
_choices = prop (Proxy :: Proxy "choices")

_bonuses
  :: forall a b r
  . Lens { bonuses :: a | r } { bonuses :: b | r } a b
_bonuses = prop (Proxy :: Proxy "bonuses")

_cardInHand :: Int -> Traversal' Player Card
_cardInHand i = _hand <<< ix i

getCard :: forall m. MonadError String m => Int -> Player -> m Card
getCard i = fromJust "cannot get card!" <<< preview (_cardInHand i)

dropCard
  :: forall m
  . MonadError String m
  => Int
  -> Player
  -> m (Array Card)
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
    >>> (_actions +~ playedCard.actions - one)
    >>> (_buys +~ playedCard.buys)

firstChoice :: Player -> Maybe Choice
firstChoice = firstOf traversed <<< view _choices

dropChoice
  :: forall m
  . MonadError String m
  => Player
  -> m Player
dropChoice = fromJust "failed to drop choice"
  <<< traverseOf _choices (deleteAt zero)

gainBonus :: Bonus -> Player -> Player
gainBonus = prependOver _bonuses

gainActions :: Actions -> Player -> Player
gainActions n = over _actions (_ + n)

gainBuys :: Buys -> Player -> Player
gainBuys n = over _buys (_ + n)

gainChoices :: Array Choice -> Player -> Player
gainChoices = flip (foldr gainChoice) <<< reverse

reactionsInHand :: Player -> Array Reaction
reactionsInHand player = catMaybes
  $ hasType CardType.Reaction `filter` player.hand
  <#> _.reaction

gainChoice :: Choice -> Player -> Player
gainChoice choice = _choices %~ (_ <> [ choice ])

updateChoice :: (Choice -> Choice) -> Player -> Player
updateChoice choiceUpdate player =
  (over _choices <<< ix 0) choiceUpdate player

hasReaction :: Player -> Boolean
hasReaction = (_ > zero) <<< length <<< reactionsInHand

purchase :: Card -> Player -> Player
purchase card = decOver _buys >>> prependOver _buying card

hasActions :: Player -> Boolean
hasActions = (_ > zero) <<< _.actions

hasActionCardsInHand :: Player -> Boolean
hasActionCardsInHand = not null <<< filter Card.isAction <<< _.hand

cash :: Player -> Int
cash player = (Card.value player.atPlay)
  + (Bonus.cashValue player.bonuses)
  + (Card.value $ Card.isTreasure `filter` player.hand)
  - (Card.cost player.buying)

drawCards
  :: forall m
  . Random m
  => Int
  -> Player
  -> m Player
drawCards n p = if n > zero
  then drawCard p >>= drawCards (n - one)
  else pure p

drawCard
  :: forall m
  . Random m
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
  , actions = one
  , buys = one
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

score :: Player -> Points
score player = foldr (+) zero $ _.victoryPoints <$> allCards player

assertHasBuys :: forall m. MonadError String m => Player -> m Player
assertHasBuys = assert (_.buys >>> (_ > zero)) "no buys!"

assertHasActions
  :: forall m
  . MonadError String m
  => Player
  -> m Player
assertHasActions = assert (_.actions >>> (_ > zero)) "no actions!"

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

upgrade :: Player -> Player
upgrade = (_atPlay %~ map Cards.upgrade)
  >>> (_buying %~ map Cards.upgrade)
  >>> (_deck %~ map Cards.upgrade)
  >>> (_discard %~ map Cards.upgrade)
  >>> (_hand %~ map Cards.upgrade)
  >>> (_toDiscard %~ map Cards.upgrade)

positivePoints :: Player -> Points
positivePoints = sum <<< (map Card.positivePoints) <<< allCards

negativePoints :: Player -> Points
negativePoints = sum <<< (map Card.negativePoints) <<< allCards

describes :: forall m. Random m => Condition -> Player -> m Boolean
describes = case _ of
  HasCard name -> pure <<< _.hand >>> any (_.name >>> (_ == name))
  HasCardType cardType -> pure <<< _.hand >>> any (hasType cardType)
  HasDiscard -> pure <<< _.discard >>> (not <<< null)
  Randomly percent -> const
    $ (_ > (percent .^ Int._toWire))
    <$> randomIntBetween zero 100

newPlayer :: Player
newPlayer =
  { deck: []
  , hand: []
  , discard: (replicate 7 (_card Cards.copper)) <> (replicate 3 (_card Cards.estate))
  , toDiscard: []
  , atPlay: []
  , buying: []
  , actions: one
  , buys: one
  , choices: []
  , bonuses : []
  }

