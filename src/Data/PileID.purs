--| ## AI Generated Module Summary (llama3.2:3b)
--|
--| ### Description
--| A custom data type for representing pile IDs with integral values.
--|
--| ### Key Concepts
--| * Bounded Enum: Understanding how to work with bounded enums and their related types (e.g. `bottom`, `top`, `Cardinality`).
--| * Generic Types: Recognizing the use of generic type classes and functions (e.g. `genericEncodeJson`, `genericDecodeJson`) for encoding and decoding data.
--| * Integral Representation: Grasping how integers are used to represent values in a bounded enum type (`PileID`).
module Domination.Data.PileID where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), pred, succ)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Safe.Coerce (coerce)

newtype PileID = PileID Int

derive instance eqPileID :: Eq PileID

derive instance ordPileID :: Ord PileID

derive instance genericPileID :: Generic PileID _

derive instance newtypePileID :: Newtype PileID _

instance encodeJsonPileID :: EncodeJson PileID where
  encodeJson = genericEncodeJson

instance decodeJsonPileID :: DecodeJson PileID where
  decodeJson = genericDecodeJson

instance showPileID :: Show PileID where
  show = genericShow

instance boundedPileID :: Bounded PileID where
  bottom = PileID 0
  top = PileID top

instance enumPileID :: Enum PileID where
  succ (PileID i) =
    if i == top
    then Nothing
    else PileID <$> succ i
  pred (PileID i) =
    if i == 0
    then Nothing
    else PileID <$> pred i

instance boundedEnumPileID :: BoundedEnum PileID where
  cardinality = Cardinality $ top + 1
  toEnum i = let pid = PileID i in
    if pid > top
    || pid < bottom
    then Nothing
    else Just pid
  fromEnum = coerce

derive newtype instance hashablePileID :: Hashable PileID
