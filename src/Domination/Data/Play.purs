module Domination.Data.Play where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Domination.Data.Choice (Choice)
import Domination.Data.Player (Player)
import Domination.Data.Stack (Stack)

data Play
  = NewGame Int
  | EndPhase Int
  | PlayCard Int Int
  | Purchase Int Player Stack
  | ResolveChoice Int Choice

derive instance genericPlay :: Generic Play _
instance showPlay :: Show Play where
  show = genericShow

