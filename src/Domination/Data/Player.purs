module Domination.Data.Player where

import Prelude

import Control.Apply (lift2)
import Data.Array (deleteAt, filter, (!!), (:))
import Data.Either (Either(..), note)
import Data.Foldable (foldr, null)
import Data.Lens.Fold (firstOf, preview)
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set)
import Data.Lens.Traversal (Traversal', traverseOf, traversed)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.Choice (Choice)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Util (dropIndices, moveOne, shuffle)

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

cardI :: Int -> Player -> Maybe Card
cardI i = preview $ _cardInHand i

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

play :: forall m. MonadEffect m => Int -> Player -> m (Either String Player)
play cardIndex player =
  case lift2 Tuple (player.hand !! cardIndex) (deleteAt cardIndex player.hand) of
  Nothing -> pure $ Left $ "error play " <> show cardIndex <> " " <> show player
  Just (Tuple playedCard hand') -> do
    mbPlayer' <- drawCards playedCard.cards player { hand = hand' }
    pure if not Card.isAction playedCard
    then Left $ "error play " <> show (Tuple playedCard hand')
    else mbPlayer' <#> \player' -> player'
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

hasActions :: Player -> Boolean
hasActions = (_ > 0) <<< _.actions

hasActionCardsInHand :: Player -> Boolean
hasActionCardsInHand = not null <<< filter Card.isAction <<< _.hand

cash :: Player -> Int
cash player = (Card.value player.atPlay)
  + (Card.value $ Card.isTreasure `filter` player.hand)
  - (Card.cost player.buying)

drawCards :: forall m. MonadEffect m => Int -> Player -> m (Either String Player)
drawCards n p = if n > 0
  then do
    mp :: Either String  Player <- drawCard p
    let (mmmp :: Either String (m (Either String Player))) = drawCards (n - 1) <$> mp
    mmp'' <- sequence mmmp
    pure $ join mmp''
  else pure $ Right p

drawCard :: forall m. MonadEffect m => Player -> m (Either String Player)
drawCard player = do
  deck <- if null player.deck
    then do
      liftEffect $ Console.log $ "Ran out of cards while drawing. Time to shuffle for player: " <> show player
      shuffle player.discard
    else pure player.deck
  let discarded = if null player.deck
    then []
    else player.discard
  let player' = player { deck = deck, discard = discarded }
  pure if null deck
  then Right $ player'
  else draw player'

draw :: Player -> Either String Player
draw p = note ("error drawing card for " <> show p) <<< moveOne _deck _hand $ p

cleanup :: forall m. MonadEffect m => Player -> m (Either String Player)
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
allCards player = player.hand <> player.deck <> player.atPlay <> player.discard <> player.toDiscard <> player.buying

score :: Player -> Int
score player = foldr (+) 0 $ map _.victoryPoints (allCards player)

