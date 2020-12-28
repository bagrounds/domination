module Player
  ( Player
  , cleanup
  , allCards
  , score
  , cash
  , hasActions
  , actionCardsInHand
  , numActionCardsInHand
  , hasActionCardsInHand
  , drawCards
  ) where

import Prelude

import Effect.Console as Console
import Data.Array (take, drop, filter, length)
import Data.Foldable (foldr, null)
import Effect.Class (class MonadEffect, liftEffect)

import Card (Card)
import Card as Card
import Util (shuffle)

type Player =
  { deck :: Array Card
  , hand :: Array Card
  , discard :: Array Card
  , toDiscard :: Array Card
  , atPlay :: Array Card
  , buying :: Array Card
  , actions :: Int
  , buys :: Int
  }

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

drawCards :: forall m. MonadEffect m => Int -> Player -> m Player
drawCards n p = if n > 0
  then drawCards (n - 1) =<< drawCard p
  else pure p

drawCard :: forall m. MonadEffect m => Player -> m Player
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
  where
    draw :: Player -> Player
    draw player = player
      { hand = take 1 player.deck <> player.hand
      , deck = drop 1 player.deck
      }

cleanup :: forall m. MonadEffect m => Player -> m Player
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

