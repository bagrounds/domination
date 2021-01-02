module Domination.Data.Stack where

import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Domination.Data.Card (Card)

type Stack =
  { card :: Card
  , count :: Int
  }

_card :: Lens' Stack Card
_card = prop (SProxy :: SProxy "card")
_count :: Lens' Stack Int
_count = prop (SProxy :: SProxy "count")

