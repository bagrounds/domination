-- | Cards that went from one pile to another, by name.
-- |
-- | The engine tells one for every card movement it makes when it resolves
-- | a choice, so the game log can say what a choice did, whatever the card.
module Domination.Data.CardMovement (CardMovement) where

import Domination.Data.Pile (Pile)

type CardMovement = { cards :: Array String, from :: Pile, to :: Pile }
