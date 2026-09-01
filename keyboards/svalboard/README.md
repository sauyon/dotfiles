# Svalboard — Hands Down Neu + Arensito symbols

Vial keymap for the Svalboard Lightly, ported from my Glove80.

## Load it

1. Open [KeyBard](https://captdeaf.github.io/keybard/) (or the Vial app) with the
   Svalboard plugged in.
2. **Load** → `SvalHandsDownNeu.vil`.

No reflash needed — Vial writes the keymap to the keyboard live.

## Layers

**Layer 0 — Hands Down Neu**

```
w f m p v   / . q " '
r s n t b   , a e i h
x c l d g   ; u o y k
```

`z` and `j` sit on the right pinky's East and West laterals. On the Glove80 they
live in the outer column, which the Svalboard doesn't have.

Note this is my Glove80's Neu, not upstream's: `;` on the right inner column
where [alanreiser.com](https://sites.google.com/alanreiser.com/handsdown/home/hands-down-neu)
publishes `-`.

**Layer 1 — Arensito symbols** (hold `MO(1)`, right thumb)

```
{ } [ ] @   & _ < > $
; / - 0 :   \ 1 ( ) =
6 7 8 9 +   * 2 3 4 5
```

**Layer 2** — nav and F-keys, untouched from Svalboard's stock config.

Thumb clusters are also stock:

```
L: ctrl tab shift bspc esc gui
R: alt enter MO(1) space MO(2) gui
```

## Host setup

Nothing to do. Hyprland scopes my Colemak variant to the two internal keyboards
by device name (`hyprland.nix`), so the Svalboard falls through to the default
plain `us` — which is what a firmware-side layout wants. Adding the Svalboard to
that `device` list would double-translate every keystroke.

## Regenerating

```sh
python3 build.py
```

`build.py` rewrites the 40 finger-cup positions on layers 0 and 1 of
`SvalCOLEMAKDHM.vil` (vendored from
[svalboard/svalboard-configs](https://github.com/svalboard/svalboard-configs)),
leaving thumbs, layer 2, and Vial metadata alone.

The interesting part is the geometry. Svalboard `.vil` layers are 10×6:

```
row 0 = left thumb        row 5 = right thumb
rows 1-4 = left  index, middle, ring, pinky
rows 6-9 = right index, middle, ring, pinky
cols     = [South, East, Center, North, West, unused]
```

Each finger has only N/C/S for its column, so the layout's inner-index column
spills onto one inward lateral per finger — `inner-top → middle`,
`inner-home → index`, `inner-bottom → ring`. That convention is Svalboard's own,
reverse-engineered from their Colemak-DHm config.

Since that mapping is a guess about matrix order that would fail *silently* —
producing a scrambled but valid-looking keymap — `build.py` refuses to emit
anything until it has run the stock Colemak-DHm block back through the same code
path and reproduced all 30 alpha positions in the shipped `.vil` exactly.

## Source

Glove80 layout `e3409150-bb22-49c0-8614-10035f3f6a04` ("Sauyon layout
2026-01-20"), layer `Base` and layer `symbols`.
