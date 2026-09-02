# Svalboard — Hands Down Neu + Arensito symbols

Vial keymap for the Svalboard Lightly, ported from my Glove80.

## Load it

1. Open [KeyBard](https://captdeaf.github.io/keybard/) with the Svalboard plugged in.
2. **Load** → `SvalHandsDownNeu.vil`.

No reflash — Vial writes the keymap live.

Needs `/dev/hidraw*` to be readable, which it isn't by default. The udev rule is
in `system/etc/udev/rules.d/92-vial.rules`. Without it, KeyBard and Vial both see
the keyboard and then silently fail to open it.

## Layers

Each finger sits in a cup with five switches: **N**orth, **S**outh, **E**ast,
**W**est and **C**enter (the plain down-press). East is screen-right on both
hands, so West is inward on the right hand and outward on the left.

### Layer 0 — Hands Down Neu

```
             W    N     C     S     E
  L pinky    tab  w     r     x     -
  L ring     ·    f     s     c     @
  L middle   (    m     n     l     v
  L index    g    p     t     d     b
  R index    ,    .     a     u     :
  R middle   /    q     e     o     )
  R ring     ;    "     i     y     &
  R pinky    z    '     h     k     j
```

The alphas are Hands Down Neu as it sits on the Glove80 — note `;` on the right
inner column where upstream publishes `-`.

Two places this departs from a literal port, both because the Svalboard has four
fingers per hand and the layout assumes five columns:

- **`g` is on the index's West.** Hands Down gives the left index the whole
  inner column (`v`/`b`/`g`). Only two fit on an index cup, so it keeps `b` and
  `g` and exiles `v` — the rarest — to the middle finger.
- **`z`, `j` and `tab` are on laterals.** They live in outer columns the
  Svalboard doesn't have. `tab` gets the left pinky's West, x=0.0, the outermost
  point of the hand.

Brackets are deliberately *not* on this layer; Arensito has them. Six symbols
*are*, placed by measured frequency rather than by feel — counted over hand-typed
prompts and shell history (agent-driven sessions excluded) and over code authored
here, per 1000 characters:

| | `-` | `:` | `(` `)` | `&` | `@` |
|---|---|---|---|---|---|
| code | 11.6 | 9.3 | 4.9 | 0.68 | 0.17 |
| hand-typed | 33.5 | 5.1 | 0.30 | 0.17 | 0.01 |

- `-` → **left pinky East.** The clear winner on both counts.
- `:` → **right index East**, the strongest free lateral. Was shift+`;`.
- `(` `)` → **left middle West / right middle East**, an outward-facing mirror
  pair, so `()` alternates hands. They earn a base key from code only.
- `&` → **right ring East.** The rarest thing on this layer bar `@`, on one of
  the worst keys here, which is the point. Ring laterals are the hardest cups to
  reach.
- `@` → **left ring East**, the other ring lateral, and the slot `g` vacated. It
  is rarer than `&` by 6× in code and 19× typed, so frequency alone would leave
  it on `MO(1)`. It gets promoted because the promotion is free: both ring
  laterals were empty, so one bad key displaces nothing and still beats two good
  ones. East is the inward of the two; West stays free.

`*` was considered and rejected: it looks like 6.7/1k in code, but 90% of that
is `**` in markdown prose, and it is 0.03/1k in anything hand-typed.

`@`'s two numbers come from a later counting pass than the other four columns —
in that same pass `&` read 1.10 and 0.19, so read the gap between the two, not
the absolute against the older columns. Most of even that 0.17 is `@types/…`
imports and `git@github.com`, which are completed rather than typed.

Each is promoted, not copied — all six are holes on layer 1, so nothing sits on
two keys.

### Layer 1 — Arensito symbols (hold `MO(1)`)

```
  { } [ ] ·   · · < > $
  ; / · 0 ·   \ 1 · · =
  6 7 8 9 +   * 2 3 4 5
```

Ported from the Glove80 `symbols` layer. `·` is a dead key — Arensito puts
`-`, `:`, `&`, `@`, `_`, `(` and `)` there, and all seven are one-key elsewhere
now (see layer 0 and the thumbs below). They're dead rather than transparent on
purpose: falling through would type `n`, `.`, `e`, `i`, `b`, `v` and `/` from
the middle of the symbol layer.

The grid above is on N/C/S; the
laterals carry the six symbols Arensito leaves out, which would otherwise cost
`MO(1)`+shift+key:

```
  ` ~ on the left pinky      ! on the left index
  % on the left ring         # on the left middle
  | on the right index       ^ on the right middle
```

`?` isn't here on purpose — it's shift+`/`, and `/` is on the base layer.

`_` gets a second seat on the **left thumb's space key**. Arensito's own `_` is
on the right index's North, the same hand as `MO(1)`; on space it's a
left-right roll instead. Space itself isn't typeable while `MO(1)` is held —
every other thumb stays transparent, so shift, ctrl, enter and backspace still
work on the layer.

### Layer 2 — navigation (hold `MO(2)`)

Arrows four-across on the right hand, innermost to outermost, matching Glove80
base-layer positions 75–78:

```
  R index  R middle  R ring  R pinky
     ←        ↓        ↑        →
```

F1–F12, home/end, pgup/pgdn, volume and play/pause fill the rest.

### Layer 15 — mouse

Svalboard's firmware switches here by itself when the pointing device moves, and
leaves on the first key that isn't on this layer. **Nothing reaches it by
keypress, by design.** Buttons 1/3/2 sit on index/middle/ring South of both
hands; `USER06` is `SV_RECALIBRATE_POINTER`.

### Thumbs

```
LEFT                                RIGHT
 row7  esc(7.4)  shift(8.5)  ctrl(9.6)     alt(11.4)  MO1(12.5)  MO2(13.6)
 row6   space(7.9) delete(9.0) gui(10.1)    enter(11.9) ralt(13.0) bspc(14.1)
```

The two rows interleave in x, so every row-6 key sits between two row-7 keys —
and the **middle** row-6 key on each hand, x=9.0 and x=13.0, is the only
position with no free side. Nothing you press in a hurry goes there. `gui` was
there originally (on both hands) and was unusable; `tab` was tried there and was
worse. They now hold `delete` and `ralt`.

`space` left and `bspc` right match Glove80 positions 69 and 74 — Svalboard's own
configs ship those mirrored.

## Printable cheatsheet

```sh
python3 cheatsheet.py   # writes cheatsheet.html
```

Open it and print: one portrait Letter page, layers 0, 1 and 2 drawn at the
hardware's own key positions, with the layer-15 note in the footer. On layers 1
and 2, keys that fall through to layer 0 are printed faint rather than left
blank — under `MO(1)` shift, ctrl, enter and backspace are all still live, and
an empty box would say otherwise.

There is no second copy of the layout in it: `cheatsheet.py` imports `build.py`
and reads the emitted layers, so the sheet cannot disagree with the `.vil`. Two
guards, on top of `build.py`'s four:

- **Labels.** Every live keycode must have a glyph. Adding a key to `build.py`
  without labelling it aborts the build instead of printing a blank box.
- **Page width.** The board must fit inside the printable width at the declared
  margins. Too wide looks fine on screen — the browser viewport is whatever size
  it is — and only shows up as a shrunken printout after you've walked to the
  printer.

## Measuring what actually gets typed

```sh
python3 freq.py            # refresh the corpus, report the promoted set
python3 freq.py '`' '~'    # rates for specific characters, with context checks
python3 freq.py --all      # every printable ASCII symbol
```

The promotions above are argued from measured frequency, and this is what
measures it: prompts you typed (sidechains and machine-injected blocks dropped),
zsh history, and the tracked text files in this repo, reported per 1000
characters.

**The corpus is cached in `~/.local/share/svalboard-freq/` and must never be
committed** — it is shell history and work prompts, and this repo is public.
What is safe to publish is the aggregate, so `freq.py` writes `freq.json` here:
per-character rates only, which say nothing about content.

Two context checks come with it, because a raw rate lies in exactly the way
`*` lied. Prompts are split inside/outside ``` fences, since a rate driven by
fenced blocks is driven by pasting rather than typing; the code corpus is split
by file extension, since a symbol confined to one file type is a symbol you type
in one context.

One caveat on comparing against the table above: those numbers are weighted
toward shell history, and `freq.py`'s combined figure is dominated by the prompt
corpus, which is ~18x larger. The ordering agrees, the absolute values do not.
Compare like with like — the per-corpus columns, not the blend.

## Regenerating

```sh
python3 build.py
```

`build.py` emits the entire `.vil`. There is no base file to patch and no
network access — every layer, keycode and piece of Vial metadata is declared in
it. It was cut over from a patch-the-Colemak-config approach by diffing against
the old output until byte-identical, so the rewrite carried nothing over by
accident.

Four guards, any of which aborts the build:

- **Geometry.** `CUP_XY` holds the physical position of all 40 finger switches,
  read out of the keyboard's own `vial.json` over the Vial protocol. The build
  asserts North is above Center, South below, East right and West left, for
  every cup. Get the column order wrong and you still emit a structurally
  perfect file with every key in the wrong place — this is the check that
  catches it, and it checks against the hardware rather than another config's
  conventions.
- **Coverage.** All 26 letters and 10 digits reachable, plus space, backspace,
  enter, tab and grave. Grave has no shift-fallback and went missing once when
  Arensito's `{` landed on top of it.
- **Symbol cost.** No printable ASCII symbol may require `MO(1)`+shift+key.
- **Duplicates.** Nothing on the base layer reachable from two positions.
  Moving a key by adding an override leaves the original behind unless it's
  blanked; that put `g` on two cups once.

Thumb rows are declared by physical x, not column index, and the build fails if
the declared positions don't match the hardware. The indices are **not** mirrored
between hands — left column 3 is at x=7.9 and mirrors right column *4* at
x=13.6, not right column 3 at x=14.1. Reasoning about them directly produced one
wrong answer already.

## Source

Glove80 layout `e3409150-bb22-49c0-8614-10035f3f6a04` ("Sauyon layout
2026-01-20"), layers `Base` and `symbols`. Its grids are transcribed into
`build.py`; nothing is fetched at build time.

Custom `USER*` keycode names (`SV_SCROLL_TOGGLE`, `SV_TOGGLE_AUTOMOUSE`, the
sniper modes, DPI controls) come from the keyboard itself — Vial command
`0xFE 0x02` returns an LZMA-compressed `vial.json` describing them.
