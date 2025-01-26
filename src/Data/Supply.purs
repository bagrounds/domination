--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| Data structures, functions, and utilities for a card game, including supply management and stack operations.
--|
--| ### Key Concepts
--| * **Supply**: A data structure representing a collection of stacks.
--| * **Stack traversal**: Traversing the Supply data structure using lenses.
--| * **Card management**: Manipulating cards within the supply, including finding, updating, and filtering stacks.
module Domination.Data.Supply where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (filter, findIndex, length, updateAt)
import Data.Foldable (find, maximumBy, sum)
import Data.Lens.Fold ((^?))
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Traversal (Traversal')
import Data.Maybe (Maybe(..))
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.CardType (CardType(..))
import Domination.Data.Cards as Cards
import Domination.Data.Points (Points)
import Domination.Data.Stack (Stack, isEmpty)
import Domination.Data.Stack (_card, negativePoints, positivePoints, upgrade) as Stack
import Util (fromJust)

type Supply = Array Stack

_stack :: Int -> Traversal' Supply Stack
_stack i = ix i

defaultSupply :: Int -> Supply
defaultSupply i = makeSupply i Cards.cardMap

makeSupply :: Int -> Array Card -> Supply
makeSupply playerCount cards = makeStack <$> cards
  where
    makeStack card = { card, count: countOf card }
    countOf card =
      if Card.hasType Victory card
      then victoryCount
      else if Card.hasType Curse card
      then curseCount
      else if Card.hasType Treasure card
      then treasureCount
      else kingdomCount
    curseCount = 10 * (playerCount - one)
    victoryCount = 4 * playerCount
    kingdomCount = 10 + ((max 0 (playerCount - 4)) * 2)
    treasureCount = 10 * playerCount

upgrade :: Supply -> Supply
upgrade = map Stack.upgrade

indexOfStack
  :: forall m
  . MonadError String m
  => Card
  -> Supply
  -> m Int
indexOfStack card stacks =
  fromJust "card not in supply!"
  $ findIndex (view Stack._card >>> (_ == card))
  $ stacks

updateStack
  :: forall m
  . MonadError String m
  => Int
  -> Stack
  -> Supply
  -> m Supply
updateStack i stack stacks = fromJust "cannot update stack!"
  $ updateAt i stack stacks

stackByName
  :: forall m
  . MonadError String m
  => String
  -> Supply
  -> m Stack
stackByName cardName supply =
  fromJust "card not in supply!" $
  find (view (Stack._card <<< Card._name) >>> (_ == cardName)) supply

nonEmptyStacks :: Supply -> Array Stack
nonEmptyStacks = filter (_.count >>> (_ > zero))

emptyStacks :: Supply -> Array Stack
emptyStacks = filter (_.count >>> (_ <= zero))

emptyStackCount :: Supply -> Int
emptyStackCount = length <<< emptyStacks

getStack
  :: forall m
  . MonadError String m
  => Int
  -> Supply
  -> m Stack
getStack i = fromJust "cannot get stack!" <<< (_ ^? _stack i)

positivePoints :: Supply -> Points
positivePoints = sum <<< map Stack.positivePoints

negativePoints :: Supply -> Points
negativePoints = sum <<< map Stack.negativePoints

highestVictoryCardStack :: Supply -> Maybe Stack
highestVictoryCardStack =
  filter isPureVictory >>> maximumBy comparingPoints
  where
    comparingPoints :: Stack -> Stack -> Ordering
    comparingPoints a b =
      a.card.victoryPoints `compare` b.card.victoryPoints
    isPureVictory :: Stack -> Boolean
    isPureVictory = _.card >>> _.types >>> (_ == [Victory])

highestVictoryCardStackIsEmpty :: Supply -> Boolean
highestVictoryCardStackIsEmpty =
  highestVictoryCardStack
  >>> map isEmpty
  >>> (_ == Just true)
