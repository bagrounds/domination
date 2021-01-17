module Domination.Data.Play where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Domination.Data.Choice (Choice)
import Domination.Data.Reaction (Reaction)

data Play
  = NewGame Int
  | EndPhase Int
  | PlayCard Int Int
  | Purchase Int Int
  | ResolveChoice Int Choice
  | React Int (Maybe Reaction)

derive instance genericPlay :: Generic Play _
instance encodeJsonPlay :: EncodeJson Play where
  encodeJson = genericEncodeJson
instance decodeJsonPlay :: DecodeJson Play where
  decodeJson = genericDecodeJson
instance showPlay :: Show Play where
  show = genericShow

