module Domination.Data.Constraint where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Lens (Lens', lens')
import Data.Tuple (Tuple(..))

data Constraint
  = UpTo Int
  | Exactly Int
  | DownTo Int

derive instance genericConstraint :: Generic Constraint _
derive instance eqConstraint :: Eq Constraint

instance showConstraint :: Show Constraint where
  show = genericShow
instance encodeJsonConstraint :: EncodeJson Constraint where
  encodeJson = genericEncodeJson
instance decodeJsonConstraint :: DecodeJson Constraint where
  decodeJson = genericDecodeJson

_n :: Lens' Constraint Int
_n = lens' case _ of
  UpTo n -> Tuple n UpTo
  Exactly n -> Tuple n Exactly
  DownTo n -> Tuple n DownTo
