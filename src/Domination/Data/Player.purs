module Domination.Data.Player where

import Prelude

import Control.Apply (lift2)
import Data.Array (deleteAt, drop, filter, head, take, (!!), (:))
import Data.Foldable (foldr, length, null)
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens')
import Data.Lens.Prism (Prism')
import Data.Lens.Record (prop)
import Data.Lens.Setter (over, set)
import Data.Lens.Traversal (Traversal', traverseOf)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.Choice (Choice)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Util (dropIndices, shuffle)

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

_hand :: Lens' Player (Array Card)
_hand = prop (SProxy :: SProxy "hand")

modifyHand :: (Array Card -> Array Card) -> Player -> Player
modifyHand = over _hand

setHand :: Array Card -> Player -> Player
setHand = set _hand

maybeModifyHand :: (Array Card -> Maybe (Array Card)) -> Player -> Maybe Player
maybeModifyHand = traverseOf _hand

dropCards :: Array Int -> Player -> Maybe Player
dropCards = maybeModifyHand <<< dropIndices

_cardInHand :: Int -> Traversal' Player Card
_cardInHand i = _hand <<< (ix i)

hasChoices :: Player -> Boolean
hasChoices = _.choices >>> not null

play :: forall m. MonadEffect m => Int -> Player -> m (Maybe Player)
play cardIndex player =
  case lift2 Tuple (player.hand !! cardIndex) (deleteAt cardIndex player.hand) of
  Nothing -> pure Nothing
  Just (Tuple playedCard hand') -> do
    mbPlayer' <- drawCards playedCard.cards player { hand = hand' }
    pure if not Card.isAction playedCard
    then Nothing
    else mbPlayer' <#> \player' -> player'
      { atPlay = playedCard : player'.atPlay
      , actions = player'.actions + playedCard.actions - 1
      , buys = player'.buys + playedCard.buys
      }

firstChoice :: Player -> Maybe Choice
firstChoice = _.choices >>> head

dropChoice :: Player -> Maybe Player
dropChoice player =
  if length player.choices < 1
  then Nothing
  else pure player { choices = drop 1 player.choices }

gainChoice :: Choice -> Player -> Player
gainChoice choice player = player { choices = choice : player.choices }

hasActions :: Player -> Boolean
hasActions = (_ > 0) <<< _.actions

actionCardsInHand :: Player -> Array Card
actionCardsInHand = filter Card.isAction <<< _.hand

numActionCardsInHand :: Player -> Int
numActionCardsInHand = length <<< actionCardsInHand

hasActionCardsInHand :: Player -> Boolean
hasActionCardsInHand = (_ > 0) <<< numActionCardsInHand

cash :: Player -> Int
cash player = (Card.value player.atPlay)
  + (Card.value $ Card.isTreasure `filter` player.hand)
  - (Card.cost player.buying)

drawCards :: forall m. MonadEffect m => Int -> Player -> m (Maybe Player)
drawCards n p = if n > 0
  then do
    mp :: Maybe Player <- drawCard p
    --let (mmmp :: Maybe (m (Maybe Player))) = drawCard <$> mp
    --mmp :: Maybe (Maybe Player) <- sequence mmmp
    --let (mp' :: Maybe Player) = join mmp
    let (mmmp :: Maybe (m (Maybe Player))) = drawCards (n - 1) <$> mp
    mmp'' <- sequence mmmp
    let mp'' = join mmp''
    pure mp''
  else pure $ Just p

drawCard :: forall m. MonadEffect m => Player -> m (Maybe Player)
drawCard player = do
  deck <- if null player.deck
  then do
    liftEffect $ Console.log $ "Ran out of cards while drawing. Time to shuffle for player: " <> show player
    shuffle player.discard
  else pure player.deck
  let discarded = if null player.deck
  then []
  else player.discard
  pure $ draw (player { deck = deck, discard = discarded })

draw :: Player -> Maybe Player
draw player = Just $ player
  { hand = take 1 player.deck <> player.hand
  , deck = drop 1 player.deck
  }

cardI :: Int -> Player -> Maybe Card
cardI i player = player.hand !! i

cleanup :: forall m. MonadEffect m => Player -> m (Maybe Player)
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

