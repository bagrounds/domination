module Domination.UI.Phase where

import Domination.Data.Phase (Phase(..))

renderText :: Phase -> String
renderText ActionPhase = "Action Phase"
renderText BuyPhase = "Buy Phase"
renderText CleanupPhase = "Cleanup Phase"

