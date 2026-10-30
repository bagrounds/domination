# Tooltips That Stay on Screen (and on Their Card)

Issue [#6](https://github.com/bagrounds/domination/issues/6) was short: card tooltips get cut off near the edge of the screen. Tap the name of a card like *Pawn* at the far right of the supply and half the description disappears.

## Why the tooltip was cut off

The tooltip was a `position: absolute` box, `10em` wide, shifted `-2em` to the left of its card. Two things went wrong:

1. **No knowledge of the viewport.** The offset was fixed, so a card near the right edge pushed its tooltip past the right edge.
2. **Scrolling containers clip.** The hand, the play area, the buying area and the supply all scroll. An absolutely positioned child cannot leave the box of a scrolling ancestor, so the tooltip was also clipped at the container's edge.

## First attempt: measure with JavaScript

The first version of this fix made the tooltip `position: fixed` and positioned it from PureScript. A pure `placeTooltip` function, covered by property tests, computed coordinates from the card's bounding rectangle and the viewport size, and the component wrote them as inline `left`/`top`.

It kept tooltips on screen, but the review pointed out two problems:

- **The tooltip stayed put.** The coordinates were measured once, when the tooltip opened. Scrolling the supply or hand moved the card but not the tooltip, so it was no longer attached to its card.
- **JavaScript was doing a stylesheet's job.** Positioning is a style concern. The fix had grown a new DOM capability, a geometry module and a dependency to do something CSS can express directly.

## The fix: CSS anchor positioning

[CSS anchor positioning](https://developer.mozilla.org/en-US/docs/Web/CSS/Guides/Anchor_positioning) lets an element be laid out relative to another element, the *anchor*, wherever that anchor is. It is Baseline in Chrome, Firefox (147+) and Safari (26+). The fix is CSS only, and the PureScript is back to exactly what it was:

```css
@supports (anchor-scope: --card-name) {
  .description {
    anchor-scope: --card-name;
  }

  .description .toggle {
    anchor-name: --card-name;
  }

  .tool-tip {
    box-sizing: border-box;
    position: fixed;
    position-anchor: --card-name;
    position-area: bottom;
    position-try-fallbacks: flip-block;
    inset: auto;
    margin: 0 8px;
    max-width: calc(100vw - 16px);
    max-height: calc(100vh - 16px);
    overflow-y: auto;
    z-index: 450;
  }
}
```

Each declaration covers one requirement:

- **`anchor-name` / `anchor-scope`.** Every card's name button is an anchor called `--card-name`. `anchor-scope` on the card's `.description` makes each tooltip find *its own* card's name rather than the last one on the page.
- **`position: fixed`.** The tooltip is laid out against the viewport, so a scrolling container can no longer clip it. Anchor positioning keeps it attached: when the supply or hand scrolls, the tooltip moves with its card.
- **`position-area: bottom`.** It sits directly below the card name, horizontally centered on it.
- **Default overflow alignment.** If the centered tooltip would cross the viewport edge, the browser shifts it back inside. `margin: 0 8px` makes that keep an 8px gap.
- **`position-try-fallbacks: flip-block`.** If there is no room below, it flips above the card name.
- **`position-visibility: anchors-visible`** (the default). If the card scrolls out of view inside its container, its tooltip is hidden instead of floating over the HUD.

Browsers without anchor positioning skip the `@supports` block and get the old tooltip unchanged.

## Verification

I checked it in Chromium at a 360×740 phone size:

| Case | Card name (left, top, right, bottom) | Tooltip |
| --- | --- | --- |
| Pawn, at the right edge | 229, 475, 323, 510 | 192, 510, **352**, 587: ends 8px from the right edge |
| Chapel, at the left edge | 37, 141, 131, 176 | **8**, 176, 168, 253: starts 8px from the left edge |
| Moat, in the middle | 133, 312, 227, 347 | 100, 347, 260, 444: centered below |
| Moat, supply scrolled 60px | 133, 252, 227, 287 | 100, **287**, 260, 384: moved with the card |
| Card name near the bottom | 133, 685, 227, 720 | 100, 588, 260, **685**: flipped above |
| Card scrolled out of the supply | 133, 103, 227, 138 | hidden |

## Takeaway

Before writing positioning code, check what the platform already does. Here, six CSS declarations replaced a geometry module, two new DOM capability methods and a dependency, and the CSS version also behaves better: the browser re-anchors the tooltip on every frame, which the measure-once code could not.

Version bumped to **v0.37.1**.
