--| ## Placement
--|
--| Pure geometry for floating elements such as card tooltips.
--|
--| A tooltip is placed next to its anchor: horizontally centered on it,
--| preferably just below it, otherwise just above it. Whatever happens,
--| the tooltip is kept inside the viewport, at least `margin` away from
--| every edge it can fit within.
module Domination.UI.Placement
  ( Size
  , Rect
  , Position
  , Placement
  , Span
  , viewportMargin
  , placeTooltip
  , spanOfStarts
  , clampInto
  , fitsInto
  ) where

import Prelude

import Data.Array (find)
import Data.Maybe (fromMaybe)

type Size = { width :: Number, height :: Number }

type Rect = { left :: Number, top :: Number, width :: Number, height :: Number }

type Position = { left :: Number, top :: Number }

type Placement =
  { viewport :: Size
  , anchor :: Rect
  , tooltip :: Size
  , margin :: Number
  }

type Span = { lower :: Number, upper :: Number }

viewportMargin :: Number
viewportMargin = 8.0

placeTooltip :: Placement -> Position
placeTooltip { viewport, anchor, tooltip, margin } =
  { left: clampInto horizontal centered
  , top: fromMaybe (clampInto vertical below)
    $ find (fitsInto vertical) [ below, above ]
  }
  where
  horizontal = spanOfStarts margin viewport.width tooltip.width
  vertical = spanOfStarts margin viewport.height tooltip.height
  centered = anchor.left + (anchor.width - tooltip.width) / 2.0
  below = anchor.top + anchor.height
  above = anchor.top - tooltip.height

spanOfStarts :: Number -> Number -> Number -> Span
spanOfStarts margin extent length =
  { lower: margin, upper: extent - margin - length }

clampInto :: Span -> Number -> Number
clampInto { lower, upper } = max lower <<< min upper

fitsInto :: Span -> Number -> Boolean
fitsInto { lower, upper } start = lower <= start && start <= upper
