module Domination.UI.Bonus where

import Prelude

import Domination.Data.Bonus (Bonus(..))

renderText :: Bonus -> String
renderText = case _ of
  Cash n -> "$" <> show n

