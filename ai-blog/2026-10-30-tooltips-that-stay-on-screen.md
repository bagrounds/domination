# Tooltips That Stay on Screen

Issue [#6](https://github.com/bagrounds/domination/issues/6) was short: card tooltips get cut off near the edge of the screen. Tap the name of a card like *Moat* at the far right of your hand and half the description disappears.

## Why the tooltip was cut off

The tooltip was a `position: absolute` box, `10em` wide, shifted `-2em` to the left of its card. Two things went wrong:

1. **No knowledge of the viewport.** The offset was fixed, so a card near the right edge pushed its tooltip past the right edge. A card near the left edge only stayed visible because of the small `-2em` overhang.
2. **Scrolling containers clip.** The hand, the play area, the buying area and the supply all scroll (`overflow-x: scroll` or `overflow-y: scroll`). An absolutely positioned child cannot leave the box of a scrolling ancestor, so the tooltip was clipped at the container's edge too, not only the screen's.

CSS alone can't solve both. A `position: fixed` tooltip escapes the scrolling containers, but then it needs coordinates. Those depend on where the card is, how big the tooltip turned out to be and how big the screen is.

## The fix: measure, then place with a pure function

### A pure placement function

`Domination.UI.Placement` is plain geometry with no DOM:

```purescript
placeTooltip :: Placement -> Position
placeTooltip { viewport, anchor, tooltip, margin } =
  { left: clampInto horizontal centered
  , top: fromMaybe (clampInto vertical below)
    $ find (fitsInto vertical) [ below, above ]
  }
```

The rules read straight from the code:

- **Horizontally**, the tooltip is centered on its anchor and then clamped into the span of valid left edges, `[margin, viewport − margin − width]`.
- **Vertically**, it tries a list of candidates in order of preference, *below* the anchor and then *above* it, and takes the first that fits. If neither fits, it clamps the "below" position into the viewport.

`clampInto` applies the lower bound last (`max lower <<< min upper`). If a tooltip is larger than the screen, its top-left corner still stays on screen, and CSS `max-width`/`max-height` with `overflow-y: auto` keep the content readable.

### Measuring in the component

The `Dom` capability gains two methods:

```purescript
boundingClientRect :: Element -> m Rect
viewportSize :: m Size
```

They are implemented for `AppM`, `DomM` and lifted through `HalogenM`, just like `stopPropagation`. The description component tags the card-name toggle and the tooltip with Halogen refs. When the tooltip opens:

1. The state flips to `visible`. Halogen re-renders synchronously, so the tooltip is now in the DOM at its natural size.
2. The component measures the anchor, the tooltip and the viewport, then stores `placeTooltip`'s result in state.
3. The tooltip is re-rendered with inline `left`/`top`.

All of this happens in one JavaScript task, so the browser never paints the tooltip in its unplaced spot.

### CSS

`.tool-tip` is now `position: fixed`, so scrolling containers can no longer clip it. Its width is capped at `calc(100vw - 16px)` and its height at `calc(100vh - 16px)`, matching the 8px `viewportMargin` on each side. Its `z-index` is raised above choice dialogues, because a flipped tooltip can now overlap them.

## Tests

The placement rules are checked with QuickCheck over random viewports, anchors (including anchors partly off screen), tooltip sizes and margins:

- a tooltip that fits the viewport is always placed entirely inside it, margin included;
- any tooltip, even one larger than the screen, keeps its top-left corner inside the margin;
- with room below the anchor, the tooltip sits exactly below it;
- with no room below but room above, it flips to exactly above;
- with room to center horizontally, it is exactly centered.

A concrete regression case covers the original report: a card at the right edge of a 360px phone screen.

## Takeaway

Generic "keep it on screen" logic is easy to get subtly wrong in event-handler code. It's easy to test once it's a total function from rectangles to a point. The DOM part shrinks to "measure three things, call a function, write two numbers".

Version bumped to **v0.37.1**.
