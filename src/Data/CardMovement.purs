-- | Cards that went from one pile to another, by name.
-- |
-- | The engine tells one for every card movement it makes when it resolves
-- | a choice, so the game log can say what a choice did, whatever the card.
module Domination.Data.CardMovement
  ( CardMovement
  , seenByEveryone
  ) where

import Prelude

import Data.Foldable (elem)
import Domination.Data.Pile (Pile)
import Domination.Data.Pile as Pile

type CardMovement = { cards :: Array String, from :: Pile, to :: Pile }

-- | Whether everyone sees which cards moved: not when they only go between
-- | a player's hand and deck, which no one else sees.
seenByEveryone :: CardMovement -> Boolean
seenByEveryone { from, to } = not (hidden from && hidden to)
  where
    hidden = (_ `elem` [ Pile.Hand, Pile.Deck ])
