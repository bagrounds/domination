module Domination.Data.Supply where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (filter, findIndex, length, updateAt)
import Data.Foldable (find, sum)
import Data.Lens.Fold ((^?))
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (review)
import Data.Lens.Traversal (Traversal')
import Domination.Data.Card (Card)
import Domination.Data.Card as Card
import Domination.Data.CardType (CardType(..))
import Domination.Data.Cards as Cards
import Domination.Data.Points (Points)
import Domination.Data.Stack (Stack, WireStack)
import Domination.Data.Stack as Stack
import Util (fromJust)

type Supply = Array Stack

type WireSupply = Array WireStack

_stack :: Int -> Traversal' Supply Stack
_stack i = ix i

_toWire :: Iso' Supply WireSupply
_toWire = iso to from where
  to = map $ view Stack._toWire
  from = map $ review Stack._toWire

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
    kingdomCount = 4 * playerCount
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

