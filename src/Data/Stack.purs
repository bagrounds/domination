module Domination.Data.Stack where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Array (catMaybes, filter, foldr, head, length, nub, replicate, reverse, (:))
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Lens (Lens)
import Data.Lens.Prism (review)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Domination.Data.Card (Card)
import Domination.Data.Cards as Cards
import Domination.Data.Points (Points)
import Domination.Data.Points as Points
import Domination.Data.WireInt (WireInt, _WireInt)
import Util (assert, decOver, (.^))

type Stack =
  { card :: Card
  , count :: Int
  }

type WireStack = Tuple WireInt WireInt

_toWire :: Iso' Stack WireStack
_toWire = iso to from where
  to = (_card %~ view Cards._toWire)
    >>> (_count %~ view _WireInt)
    >>> toTuple
  from = fromTuple
    >>> (_card %~ review Cards._toWire)
    >>> (_count %~ review _WireInt)
  toTuple { card, count } = Tuple card count
  fromTuple (Tuple card count) = { card, count }

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

