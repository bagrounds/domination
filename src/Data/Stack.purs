module Domination.Data.Stack where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Domination.Data.Card (Card)
import Util (assert, decOver)

type Stack =
  { card :: Card
  , count :: Int
  }

_card :: Lens' Stack Card
_card = prop (SProxy :: SProxy "card")
_count :: Lens' Stack Int
_count = prop (SProxy :: SProxy "count")

take :: Stack -> Stack
take = decOver _count

assertNotEmpty :: forall m. MonadError String m => Stack -> m Stack
assertNotEmpty = assert (_.count >>> (_ > 0)) "stack is empty!"

