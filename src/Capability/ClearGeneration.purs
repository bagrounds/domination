-- | Keeps deleted data deleted. The Storage panel advances a counter in
-- | localStorage when it deletes data that a page may hold in memory; a page
-- | that started under an older counter stops writing and reloads instead.
module Domination.Capability.ClearGeneration
  ( adopt
  , advance
  , generationKey
  , isCurrent
  ) where

import Prelude (Unit)

import Effect (Effect)

-- | The localStorage key holding the counter.
foreign import generationKey :: String

-- | Whether the counter still has the value this page started with.
foreign import isCurrent :: Effect Boolean

-- | Increments the counter; false when the browser refused the write.
foreign import advance :: Effect Boolean

-- | Takes the current counter as this page's own, for a page that deleted
-- | data it doesn't hold.
foreign import adopt :: Effect Unit
