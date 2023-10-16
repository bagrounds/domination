module Domination.Data.PlayerID where

import Data.Bounded (class Bounded, bottom, top)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultFromEnum, defaultToEnum, succ)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (class Ord, (<), (>))
import Data.Ring ((-))
import Data.Semiring ((+))
import Safe.Coerce (coerce)

newtype PlayerID = PlayerID Int

derive instance eqPlayerID :: Eq PlayerID

derive instance ordPlayerID :: Ord PlayerID

derive newtype instance hashablePlayerID :: Hashable PlayerID

instance boundedPlayerID :: Bounded PlayerID where
  bottom = PlayerID 0
  top = PlayerID top

instance enumPlayerID :: Enum PlayerID where
  succ (PlayerID i) =
    if (PlayerID $ i + 1) > top
    then Nothing
    else Just (PlayerID $ i + 1)
  pred (PlayerID i) =
    if (PlayerID $ i - 1) < bottom
    then Nothing
    else Just (PlayerID $ i - 1)

instance boundedEnumPlayerID :: BoundedEnum PlayerID where
  cardinality = Cardinality $ coerce (top + 1)
  toEnum = defaultToEnum
  fromEnum = defaultFromEnum

nextPlayer :: PlayerID -> PlayerID
nextPlayer p = fromMaybe bottom $ succ p
