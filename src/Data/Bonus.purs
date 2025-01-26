--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| This module defines a data type Bonus representing money, along with associated functions for encoding, decoding, and manipulating its value.
--|
--| ### Key Concepts
--| * Data types and encoding/decoding schemes for representing bonus data.
--| * Array folding and aggregation operations.
--| * Generic programming with Argonaut's decoding and encoding functionality.
module Domination.Data.Bonus where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (foldr)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Show.Generic (genericShow)

data Bonus
  = Cash Int

cashValue :: Array Bonus -> Int
cashValue = foldr (\a b -> cashValue1 a + b) zero

cashValue1 :: Bonus -> Int
cashValue1 = case _ of
  Cash c -> c

derive instance genericBonus :: Generic Bonus _

derive instance eqBonus :: Eq Bonus

instance showBonus :: Show Bonus where show = genericShow

instance encodeJsonBonus :: EncodeJson Bonus where
  encodeJson a = genericEncodeJson a

instance decodeJsonBonus :: DecodeJson Bonus where
  decodeJson a = genericDecodeJson a

instance hashableBonus :: Hashable Bonus where
  hash = cashValue1
