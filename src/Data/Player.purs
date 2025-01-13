--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| This is a Haskell code snippet that appears to be part of a game implementation. It defines various functions and data structures for managing a player's cards, actions, cash, and other game state. Here's a breakdown of the code:
--|
--| **Data Structures**
--|
--| * `Player`: represents a player with various attributes such as:
--| 	+ `deck`: an array of cards
--| 	+ `hand`: an array of cards
--| 	+ `discard`: an array of cards to be discarded
--| 	+ `toDiscard`: an array of cards to be discarded (i.e., cards that are not in the player's hand)
--| 	+ `atPlay`: an array of cards played during the current turn
--| 	+ `buying`: an array of cards being bought
--| 	+ `actions`: a count of actions available
--| 	+ `buys`: a count of buys available
--| 	+ `choices`: an array of choices (not clear what this represents)
--| 	+ `reaction`: a value representing the player's reaction to certain events
--| 	+ `bonuses`: an array of bonuses received
--|
--| **Functions**
--|
--| * `cleanup`: draws 5 cards from the deck and removes them from the player's hand, discard pile, atPlay pile, and buying pile. It also updates the player's cash.
--| * `drawCards`: draws n cards from the deck and adds them to the player's hand if they are possible (i.e., not already in the hand).
--| * `drawCard`: draws a single card from the deck or returns the existing hand if it is empty.
--| * `allCards`: returns an array of all cards owned by the player, including those in their hand, deck, atPlay pile, discard pile, and buying pile.
--| * `score`: calculates the total score for the player based on their victory points.
--| * `positivePoints` and `negativePoints`: calculate the sum of positive and negative point values for each card in the player's collection.
--| * `upgrade`: upgrades all cards in the player's hand, deck, discard pile, buying pile, and atPlay pile.
--| * `assertHasBuys`, `assertHasActions`, and `assertHasCash` are assertion functions that check whether a player has sufficient buys, actions, or cash, respectively.
--|
--| **Rules**
--|
--| The code defines several rules using the `Rule` type, which is not defined in this snippet. These rules appear to be related to game mechanics, such as drawing cards, buying cards, and resolving interactions between players.
--|
--| Some notable rules include:
--|
--| * `handSizeIs`: checks whether a player has a certain number of cards in their hand.
--| * `hasCash`: checks whether a player has sufficient cash to perform an action or make a purchase.
--| * `upgrade`: upgrades all cards in the player's collection.
--|
--| Overall, this code appears to be implementing a game with complex interactions and rules, which is why it is quite lengthy. The use of Haskell's functional programming paradigm allows for concise and expressive definitions of these rules and data structures.
--|
--| ### Key Concepts
--| This is a Haskell code that defines a set of rules and functions for a card game, likely a simple version of the popular game "Coloretto". Here's a breakdown of the code:
--|
--| **Player Structure**
--|
--| The `Player` structure represents a player in the game. It has several fields, including:
--|
--| * `deck`: a list of cards in the deck
--| * `hand`: a list of cards in the hand
--| * `discard`: a list of cards that have been discarded
--| * `toDiscard`: an empty list (used for future cards to be added)
--| * `atPlay`: a list of cards played during the current turn
--| * `buying`: a list of cards purchased by the player
--| * `actions`: a count of actions available to the player
--| * `buys`: a count of buys available to the player
--| * `choices`: an empty list (used for future choices)
--| * `reaction`: a value representing the reaction of the player
--|
--| **Rule Definitions**
--|
--| The code defines several rules, including:
--|
--| * `hasCash`: checks if the player has enough cash to play a card with a specific cost.
--| * `handSizeIs`: checks if the number of cards in the hand matches a certain condition.
--| * `upgrade`: upgrades all cards in the deck, discard pile, and atPlay pile.
--|
--| **Functions**
--|
--| The code defines several functions, including:
--|
--| * `drawCards`: draws a specified number of cards from the deck and updates the player's hand.
--| * `drawCard`: draws one card from the deck and updates the player's hand.
--| * `cleanup`: cleans up the game by drawing 5 cards from the deck and removing all cards that have been played during the current turn.
--| * `allCards`: returns a list of all cards in the game, including those in the player's hand, deck, atPlay pile, discard pile, toDiscard pile, and buying pile.
--| * `score`: calculates the score of the game by summing up the positive points of all cards in the game.
--|
--| **Other Functions**
--|
--| The code also defines several other functions, including:
--|
--| * `assertHasBuys`, `assertHasActions`, `assertHasCash`, which assert that certain conditions are met and raise an error if they are not.
--| * `upgradeAllCards` which upgrades all cards in the deck, discard pile, and atPlay pile.
--|
--| Note that this is just a partial explanation of the code, and there may be other functions or rules that are not explicitly mentioned here.
module Domination.Data.Player where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (catMaybes, deleteAt, filter, head, length, replicate, reverse)
import Data.Foldable (foldr, null, sum)
import Data.Lens.Fold (firstOf, preview)
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Lens (Lens)
import Data.Lens.Prism (review)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, (%~), (+~), (.~))
import Data.Lens.Traversal (Traversal', traverseOf, traversed)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Domination.Capability.Random (class Random, shuffle)
import Domination.Data.Actions (Actions)
import Domination.Data.Bonus (Bonus)
import Domination.Data.Bonus (cashValue) as Bonus
import Domination.Data.Buys (Buys)
import Domination.Data.Card (Card, _card)
import Domination.Data.Card (cost, isAction, isTreasure, negativePoints, positivePoints, value) as Card
import Domination.Data.Cards as Cards
import Domination.Data.Choice (Choice)
import Domination.Data.Choice (isAttack) as Choice
import Domination.Data.Points (Points)
import Domination.Data.Reaction (Reaction)
import Domination.Data.Stack (sortByStacks)
import Domination.Data.Wire.Bonus (WireBonus)
import Domination.Data.Wire.Bonus (_toWire) as Bonus
import Domination.Data.Wire.Card (_toWire) as Card
import Domination.Data.Wire.Choice (WireChoice)
import Domination.Data.Wire.Choice (_toWire) as Choice
import Domination.Data.Wire.Int (WireInt)
import Relationship (Relationship, is)
import Rule (Rule, check, (!>), (<@!))
import Util (assert, decOver, dropIndices, fromJust, moveOne, prependOver, (:~), (<$>~))

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
  , reaction :: Maybe Reaction
  , toDiscard :: Array Card
  }

type WirePlayer =
  (Tuple Actions
  (Tuple (Array WireInt) -- atPlay
  (Tuple (Array WireBonus) -- bonuses
  (Tuple (Array WireInt) -- buying
  (Tuple Buys
  (Tuple (Array WireChoice) -- choices
  (Tuple (Array WireInt) -- deck
  (Tuple (Array WireInt) -- discard
  (Tuple (Array WireInt) -- hand
  (Tuple (Maybe Reaction) (Array WireInt)))))))))))

_toWire :: Iso' Player WirePlayer
_toWire = iso to from where
  to = (_choices <$>~ view Choice._toWire)
    >>> (_bonuses <$>~ view Bonus._toWire)
    >>> (_deck <$>~ view Card._toWire)
    >>> (_hand <$>~ view Card._toWire)
    >>> (_discard <$>~ view Card._toWire)
    >>> (_toDiscard <$>~ view Card._toWire)
    >>> (_atPlay <$>~ view Card._toWire)
    >>> (_buying <$>~ view Card._toWire)
    >>> toTuple
  from = fromTuple
    >>> (_choices <$>~ review Choice._toWire)
    >>> (_bonuses <$>~ review Bonus._toWire)
    >>> (_deck <$>~ review Card._toWire)
    >>> (_hand <$>~ review Card._toWire)
    >>> (_discard <$>~ review Card._toWire)
    >>> (_toDiscard <$>~ review Card._toWire)
    >>> (_atPlay <$>~ review Card._toWire)
    >>> (_buying <$>~ review Card._toWire)
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

_deck
  :: forall a b r
  . Lens { deck :: a | r } { deck :: b | r } a b
_deck = prop (SProxy :: SProxy "deck")
_hand
  :: forall a b r
  . Lens { hand :: a | r } { hand :: b | r } a b
_hand = prop (SProxy :: SProxy "hand")
_discard
  :: forall a b r
  . Lens { discard :: a | r } { discard :: b | r } a b
_discard = prop (SProxy :: SProxy "discard")
_toDiscard
  :: forall a b r
  . Lens { toDiscard :: a | r } { toDiscard :: b | r } a b
_toDiscard = prop (SProxy :: SProxy "toDiscard")
_atPlay
  :: forall a b r
  . Lens { atPlay :: a | r } { atPlay :: b | r } a b
_atPlay = prop (SProxy :: SProxy "atPlay")
_buying
  :: forall a b r
  . Lens { buying :: a | r } { buying :: b | r } a b
_buying = prop (SProxy :: SProxy "buying")
_actions
  :: forall a b r
  . Lens { actions :: a | r } { actions :: b | r } a b
_actions = prop (SProxy :: SProxy "actions")
_buys
  :: forall a b r
  . Lens { buys :: a | r } { buys :: b | r } a b
_buys = prop (SProxy :: SProxy "buys")
_choices
  :: forall a b r
  . Lens { choices :: a | r } { choices :: b | r } a b
_choices = prop (SProxy :: SProxy "choices")
_reaction :: Traversal' Player Reaction
_reaction = prop (SProxy :: SProxy "reaction") <<< _Just
_bonuses
  :: forall a b r
  . Lens { bonuses :: a | r } { bonuses :: b | r } a b
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
  . MonadError String m
  => Random m
  => Int -> Player -> m Player
drawCards n p = if n > zero
  then drawCard p >>= drawCards (n - one)
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
  let
    player' = player { deck = deck, discard = discarded }
    player'' = drawIfPossible player'
    player''' = over _hand sortByStacks player''
  pure player'''

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

assertHasActions :: forall m. MonadError String m => Player -> m Player
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

handSizeIs :: Relationship -> Int -> Rule Player
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
  , reaction : Nothing
  , bonuses : []
  }
