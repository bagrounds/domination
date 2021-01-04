module Domination.Data.Play where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Domination.Data.Choice (Choice)

data Play
  = NewGame Int
  | EndPhase Int
  | PlayCard Int Int
  | Purchase Int Int
  | ResolveChoice Int Choice

derive instance genericPlay :: Generic Play _
instance showPlay :: Show Play where
  show = genericShow

