--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A data type for unique card IDs with various derived instance methods for equality, ordering, hashing, and enumerating possible values.
--|
--| ### Key Concepts
--| * **CardID data type**: A newtype alias for an Int representing a unique identifier.
--| * **BoundedEnum**: An instance of the BoundedEnum class for CardID, defining its bounds and ordering properties.
--| * **Enum and Eq instances**: Instances that allow CardID to be treated as an enum and comparable with other values.
module Domination.Data.CardID where

import Data.Bounded (class Bounded, bottom, top)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultFromEnum, defaultToEnum)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord, (<), (>))
import Data.Ring ((-))
import Data.Semiring (zero, (+))
import Safe.Coerce (coerce)

newtype CardID = CardID Int

derive instance eqCardID :: Eq CardID

derive instance ordCardID :: Ord CardID

derive newtype instance hashableCardID :: Hashable CardID

instance boundedCardID :: Bounded CardID where
  bottom = CardID zero
  top = CardID top

instance enumCardID :: Enum CardID where
  succ (CardID i) =
    if (CardID $ i + 1) > top
    then Nothing
    else Just (CardID $ i + 1)
  pred (CardID i) =
    if (CardID $ i - 1) < bottom
    then Nothing
    else Just (CardID $ i - 1)

instance boundedEnumCardID :: BoundedEnum CardID where
  cardinality = Cardinality $ coerce (top + 1)
  toEnum = defaultToEnum
  fromEnum = defaultFromEnum
