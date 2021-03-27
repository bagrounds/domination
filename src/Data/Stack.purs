module Domination.Data.Stack where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (catMaybes, filter, foldr, head, length, nub, replicate, reverse, (:))
import Data.Lens.Lens (Lens)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Symbol (SProxy(..))
import Domination.Data.Card (Card)
import Domination.Data.Cards as Cards
import Domination.Data.Points (Points)
import Domination.Data.Points as Points
import Util (assert, decOver)

type Stack =
  { card :: Card
  , count :: Int
  }

toCards :: Stack -> Array Card
toCards { count, card } = replicate count card

_card
  :: forall a b r
  . Lens { card :: a | r } { card :: b | r } a b
_card = prop (SProxy :: SProxy "card")
_count
  :: forall a b r
  . Lens { count :: a | r } { count :: b | r } a b
_count = prop (SProxy :: SProxy "count")

take :: Stack -> Stack
take = decOver _count

isEmpty :: Stack -> Boolean
isEmpty = _.count >>> (_ <= 0)

assertNotEmpty :: forall m. MonadError String m => Stack -> m Stack
assertNotEmpty = assert (_.count >>> (_ > zero)) "stack is empty!"

stackCards :: Array Card -> Array Stack
stackCards cards = catMaybes (foldr f [] names)
  where
    names = nub $ _.name <$> reverse cards
    f name stacks =
      ({ card: _, count: length cards' } <$> head cards')
        : stacks
      where
        cards' = (_.name >>> (_ == name)) `filter` cards

upgrade :: Stack -> Stack
upgrade = _card %~ Cards.upgrade

points :: Stack -> Points
points { card, count } = card.victoryPoints * Points.points count

positivePoints :: Stack -> Points
positivePoints = max zero <<< points

negativePoints :: Stack -> Points
negativePoints = min zero <<< points

