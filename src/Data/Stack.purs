module Domination.Data.Stack where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Lens.Getter (view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Lens (Lens')
import Data.Lens.Prism (review)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%~))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Domination.Data.Card (Card)
import Domination.Data.Cards as Cards
import Util (assert, decOver)

type Stack =
  { card :: Card
  , count :: Int
  }

type WireStack = Tuple Int Int

_toWire :: Iso' Stack WireStack
_toWire = iso to from where
  to = (prop _card %~ view Cards._toWire) >>> toTuple
  from = fromTuple >>> (prop _card %~ review Cards._toWire)
  _card = SProxy :: SProxy "card"
  toTuple { card, count } = Tuple card count
  fromTuple (Tuple card count) = { card, count }

_card :: Lens' Stack Card
_card = prop (SProxy :: SProxy "card")
_count :: Lens' Stack Int
_count = prop (SProxy :: SProxy "count")

take :: Stack -> Stack
take = decOver _count

assertNotEmpty :: forall m. MonadError String m => Stack -> m Stack
assertNotEmpty = assert (_.count >>> (_ > 0)) "stack is empty!"

