module Domination.Data.Player where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array (deleteAt, filter, (:))
import Data.Foldable (foldr, null)
import Data.Lens.Fold (firstOf, preview)
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set)
import Data.Lens.Traversal (Traversal', traverseOf, traversed)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Domination.Capability.Random (class Random, shuffle)
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.Choice (Choice)
import Util (assert, decOver, dropIndices, fromJust, moveOne, prependOver)

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

_cardInHand :: Int -> Traversal' Player Card
_cardInHand i = _hand <<< ix i

getCard :: forall m. MonadError String m => Int -> Player -> m Card
getCard i = fromJust "cannot get card!" <<< preview (_cardInHand i)

getHand :: Player -> Array Card
getHand = view _hand

setHand :: Array Card -> Player -> Player
setHand = set _hand

modifyHand :: (Array Card -> Array Card) -> Player -> Player
modifyHand = over _hand

maybeModifyHand :: (Array Card -> Maybe (Array Card)) -> Player -> Maybe Player
maybeModifyHand = traverseOf _hand

dropCards :: Array Int -> Player -> Maybe Player
dropCards = maybeModifyHand <<< dropIndices

hasChoices :: Player -> Boolean
hasChoices = _.choices >>> not null

play
  :: forall m
  . MonadError String m
  => Random m
  => Int -> Player -> m Player
play cardIndex player = do
  playedCard <- getCard cardIndex player
  hand' <- case (deleteAt cardIndex player.hand) of
    Nothing -> throwError "cannot delete card index from hand"
    Just h -> pure h
  player' :: Player <- (drawCards playedCard.cards (player { hand = hand' }))
  if not Card.isAction playedCard
  then throwError $ "cannot play non-action card"
  else pure player'
    { atPlay = playedCard : player'.atPlay
    , actions = player'.actions + playedCard.actions - 1
    , buys = player'.buys + playedCard.buys
    }

firstChoice :: Player -> Maybe Choice
firstChoice = firstOf traversed <<< view _choices

dropChoice :: Player -> Maybe Player
dropChoice = traverseOf _choices $ deleteAt 0

gainChoice :: Choice -> Player -> Player
gainChoice choice = over _choices $ (choice : _)

purchase :: Card -> Player -> Player
purchase card = decOver _buys >>> prependOver _buying card

hasActions :: Player -> Boolean
hasActions = (_ > 0) <<< _.actions

hasActionCardsInHand :: Player -> Boolean
hasActionCardsInHand = not null <<< filter Card.isAction <<< _.hand

cash :: Player -> Int
cash player = (Card.value player.atPlay)
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
assertHasBuys = assert "no buys!" (_.buys >>> (_ > 0))

assertHasActions :: forall m. MonadError String m => Player -> m Player
assertHasActions = assert "no actions!" (_.actions >>> (_ > 0))

assertHasCash :: forall m. MonadError String m => Int -> Player -> m Player
assertHasCash i = assert "not enough treasure!" $ cash >>> (_ >= i)

