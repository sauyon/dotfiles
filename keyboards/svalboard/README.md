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
  L pinky    tab  w     r     x     "
  L ring     )    v     s     c     =
  L middle   *    m     n     f     l
  L index    g    p     t     d     b
  R index    .    `     a     u     '
  R middle   -    q     e     o     _
  R ring     ,    /     i     y     (
  R pinky    z    :     h     k     j
```

The alphas are Hands Down Neu as it sits on the Glove80, with three exceptions
and one structural departure.

- **`f`, `v` and `l` rotate.** `f` takes the left middle's South, `v` the ring's
  North, `l` the middle's East. `f` is one of the config's high-frequency double
  consonants and a ring North is a position its constraint table scores 10 --
  `ff` is 11.5% of `f` presses here and 12.0% in English, so this is not an
  artifact of one corpus. These three keys are worth 264 points; the next ten
  alpha keys are worth about 11 each.
- **`g` is on the index's West.** Hands Down gives the left index the whole inner
  column (`v`/`b`/`g`). Only two fit on an index cup, so it keeps `b` and `g`.
- **`z` and `tab` are on laterals.** They live in outer columns the Svalboard
  does not have. `tab` gets the left pinky's West, x=0.0, the outermost point of
  the hand -- and it stays there by hand, because tab has no frequency in the
  corpus, so the optimizer reads it as free and parks it on the worst key.

### The thirteen symbols are measured, not argued

26 alphas and tab occupy 27 of the 40 finger keys. The remaining **13 are symbol
seats**, and which 13 symbols fill them is decided by `freq.py` against the
corpus actually typed here -- agent prompts, shell history, Slack, fourteen
years of Discord, and the text tracked in this repo. Per 1000 characters:

| in | `-` | `.` | `/` | `'` | `` ` `` | `,` | `*` | `:` | `_` | `"` | `=` | `)` | `(` |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| | 14.07 | 9.20 | 9.19 | 7.78 | 5.52 | 5.40 | 4.42 | 4.29 | 4.24 | 3.27 | 2.11 | 1.68 | 1.63 |

| out | `;` | `&` | `~` | `@` |
|---|---|---|---|---|
| | 1.14 | 0.19 | 0.18 | 0.05 |

Every symbol off this layer costs two keystrokes -- a shift pair or `MO(1)` --
and every one on it costs one, so the right 13 are simply the 13 most frequent.
`` ` `` alone is **102x `@`**. The trade is worth **14.74 keystrokes per 1000
characters**, about 1.5% of everything typed.

`(` and `)` both make the cut, at ranks 12 and 13, so the pair stays together.

**Two earlier arguments on this layer have expired**, and both are worth
recording because they were wrong in instructive ways:

- `@` and `~` were promoted onto the ring outer laterals on the reasoning that
  those keys were empty, so a symbol there "displaces nothing and still beats
  two". It does not. `config/keyboard/sval.yml` scores both of those laterals
  **99**, against 2+3 for a hold plus a decent key -- so each promotion was a
  loss of roughly 17 per 1000 characters, not a free win. A key nobody wants is
  not the same as a key that is free.
- `~` earned its lateral on a count of **zero** consecutive repeats. That was
  true of the corpus available at the time. It is **38.3%** now: the Discord
  export arrived carrying `~~strikethrough~~`. The measurement was honest and
  the conclusion still expired, which is the argument for deriving this from a
  corpus that gets rebuilt rather than from a comment.

Placement within the 13 seats is the annealer's, not hand-argued. With the
alphas pinned, three of four restarts returned this arrangement identically --
which is what makes it worth trusting, where four earlier unconstrained runs
disagreed with each other from identical inputs and were discarded.

`?` and `~` stay reachable as shift+`/` and shift+`` ` ``, both now on this
layer. `;` is on `MO(1)`; `@` and `&` moved to the left pinky's laterals there,
because `check_symbols()` fails the build on any symbol costing three keys.

Against the layout this replaces, scored on the same corpus and config:
**838.50 to 235.22**, and Character Constraints -- every hand-authored placement
rule in the evaluation config -- reaches **zero** for the first time.

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
  ` on the left pinky        ! on the left index
  % on the left ring         # on the left middle
  | on the right index       ^ on the right middle
```

`~` used to sit beside `` ` `` on the left pinky's East, paired with its
unshifted partner. It is on the base layer now, and its old lateral is dead
rather than transparent — falling through would type `-`.

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

`MO(2)` is on the right thumb's **nail** so that `alt` can be held with it — see
"Thumbs" below. Everything else on the layer is a right-hand finger, so shift,
ctrl and GUI reach it from the left thumb without contortion.

### Layer 15 — mouse

Svalboard's firmware switches here by itself when the pointing device moves, and
leaves on the first key that isn't on this layer. **Nothing reaches it by
keypress, by design.** Buttons 1/3/2 sit on index/middle/ring South of both
hands; `USER06` is `SV_RECALIBRATE_POINTER`.

### Thumbs

The cluster is not six keys laid out in space. It is six switches hit by
different **parts of the same thumb**, and the `.vil` column index — which *is*
mirrored between hands, unlike the x coordinate — names them:

| col | part | left | right |
|---|---|---|---|
| 0 | knuckle | ctrl | **alt** (one-shot) |
| 1 | nail | gui | **`MO(2)`** |
| 2 | down | shift | `MO(1)` |
| 3 | pad | space | bspc |
| 4 | up | esc | enter |
| 5 | double-down | delete | ralt |

`pad` is the community favourite and holds the most-pressed thumb key on each
hand. `down` takes the heaviest hold. `knuckle` is the worst key on the cluster
and is exactly where a modifier belongs. `up` strains under heavy use, so it
gets taps rather than holds.

**One thumb holds one of these at a time.** The exception is a *fat-finger*
pair, of which knuckle+nail is the reliable one — the only way a single thumb
holds two thumb keys at once.

That is the whole reason `MO(2)` sits on the nail. It used to be on `up`, and
that made `alt`+arrow — `alt+shift+down`, `ctrl+alt+left`, all of them — not
awkward but **unpressable**: alt is on the knuckle, and no thumb reaches knuckle
and up together. Moving `MO(2)` to the nail puts it on alt's fat-finger partner
and costs only `enter`, which took `up` in exchange. Enter is a tap and pays
`up`'s strain once; `MO(2)` is a hold and was paying it for the length of every
arrow key.

**Alt is a one-shot**, `OSM(MOD_LALT)`, because one chord partner is not enough.
The knuckle pairs with the nail and nothing else, so alt can chord with exactly
one right-thumb key — and `MO(2)`, `enter` and `bspc` all want to be it. Sticky
alt sidesteps the whole problem: tap it and it holds for the next key, so
`M-RET` and `M-DEL` work without a chord at all. `M-DEL` has never been
available on this layout by any other route — backspace is on the pad. Holding
it is unchanged, so `alt+tab` still cycles.

That is one of the three fixes the Svalboard community names for this squeeze.
The other two don't fit here: **one layer hold per thumb** (this layout has both
on the right) and **bottom-row mods**, "because south is so good on sval" —
South is alphas here. The underlying rule being broken is *modifiers on one
thumb, layer switches on the other*: `alt`, `MO(1)` and `MO(2)` are all on the
right. Shift, ctrl and GUI escape it by being left-thumb keys. Alt can't follow
them; the left thumb's six seats are full.

Note `OSM(MOD_LALT)`, not `OSM(KC_LALT)` — Vial accepts both, but they are
different keycodes (`0x52A8` vs `0x52E3`) and the latter reloads as raw hex.

`double-down` is not a sixth position. It is the `down` key pressed harder, the
DataHand deep press. That is why nothing you press in a hurry goes there — `gui`
sat on both and was unusable, `tab` was tried and was worse. They now hold
`delete` and `ralt`. Those two slots were found empirically, as "the only
positions with no free side", well before the mechanism had a name.

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
