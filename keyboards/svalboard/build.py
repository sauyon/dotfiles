#!/usr/bin/env python3
"""Generate a Svalboard Vial keymap (.vil) for Hands Down Neu + Arensito symbols.

Emits the whole file. There is no base .vil to patch and no network access --
every layer, every keycode and all the Vial metadata is declared below.

Source of truth for the alphas and symbols is my Glove80, my.glove80.com layout
e3409150-bb22-49c0-8614-10035f3f6a04, layers `Base` and `symbols`. Its grids are
transcribed into the constants below.

Run `python3 build.py` to write SvalHandsDownNeu.vil. The build refuses to emit
anything unless check_geometry() and check_coverage() both pass.
"""

import json
import pathlib
import sys

HERE = pathlib.Path(__file__).parent
OUT = HERE / "SvalHandsDownNeu.vil"

# uid identifies the physical keyboard. Read off mine over the Vial protocol
# (raw HID usage page 0xFF60, command 0xFE 0x00). Vial warns on a mismatch.
UID = 5199957870438586395

LAYERS = 16
ROWS = 10
COLS = 6

# --- Svalboard geometry ------------------------------------------------------
#
# layout[layer] is 10 rows x 6 columns.
#
#   row 0 = left thumb cluster      row 5 = right thumb cluster
#   rows 1-4 = left  index, middle, ring, pinky
#   rows 6-9 = right index, middle, ring, pinky
#
# A finger row's first five columns are the five switches of that cup, in
# absolute directions -- East is screen-right on both hands. Column 5 is unused
# on finger rows and must be -1; thumb rows use all six.

S, E, C, N, W = 0, 1, 2, 3, 4

L_THUMB, R_THUMB = 0, 5
L_INDEX, L_MIDDLE, L_RING, L_PINKY = 1, 2, 3, 4
R_INDEX, R_MIDDLE, R_RING, R_PINKY = 6, 7, 8, 9

THUMB_ROWS = (L_THUMB, R_THUMB)
FINGER_ROWS = (L_INDEX, L_MIDDLE, L_RING, L_PINKY,
               R_INDEX, R_MIDDLE, R_RING, R_PINKY)

FINGER_NAMES = {
    L_INDEX: "L index", L_MIDDLE: "L middle", L_RING: "L ring", L_PINKY: "L pinky",
    R_INDEX: "R index", R_MIDDLE: "R middle", R_RING: "R ring", R_PINKY: "R pinky",
}
DIR_NAMES = {S: "South", E: "East", C: "Center", N: "North", W: "West"}

# Physical key positions, in KLE units, read out of the keyboard's own vial.json
# (Vial command 0xFE 0x02, LZMA-compressed). This is the hardware's account of
# itself, not a convention borrowed from some other config -- which is what
# makes check_geometry() below worth running.
CUP_XY = {
    L_INDEX:  {S: (9.5, 3.5),  E: (10.5, 2.5), C: (9.5, 2.5),  N: (9.5, 1.5),  W: (8.5, 2.5)},
    L_MIDDLE: {S: (7.0, 2.0),  E: (8.0, 1.0),  C: (7.0, 1.0),  N: (7.0, 0.0),  W: (6.0, 1.0)},
    L_RING:   {S: (3.5, 2.0),  E: (4.5, 1.0),  C: (3.5, 1.0),  N: (3.5, 0.0),  W: (2.5, 1.0)},
    L_PINKY:  {S: (1.0, 3.5),  E: (2.0, 2.5),  C: (1.0, 2.5),  N: (1.0, 1.5),  W: (0.0, 2.5)},
    R_INDEX:  {S: (14.5, 3.5), E: (15.5, 2.5), C: (14.5, 2.5), N: (14.5, 1.5), W: (13.5, 2.5)},
    R_MIDDLE: {S: (17.0, 2.0), E: (18.0, 1.0), C: (17.0, 1.0), N: (17.0, 0.0), W: (16.0, 1.0)},
    R_RING:   {S: (20.5, 2.0), E: (21.5, 1.0), C: (20.5, 1.0), N: (20.5, 0.0), W: (19.5, 1.0)},
    R_PINKY:  {S: (23.0, 3.5), E: (24.0, 2.5), C: (23.0, 2.5), N: (23.0, 1.5), W: (22.0, 2.5)},
}

# Thumb keys, same source. Reason about these by x, never by column index -- the
# indices are NOT mirrored between hands (left col 3 at x=7.9 is the mirror of
# right col 4 at x=13.6, not right col 3 at x=14.1).
#
# These x/y are a flattened KLE projection and they LIE about the physical
# cluster. The six thumb switches are not laid out in space for a thumb to slide
# across: they are struck by different PARTS of the one thumb, and the column
# index -- which is mirrored, unlike x -- is what names them:
#
#   col 0  knuckle       col 3  pad
#   col 1  nail          col 4  up
#   col 2  down          col 5  double-down
#
# `double-down` is not a sixth position at all. It is col 2 pressed harder --
# the DataHand deep press, inherited. That is the real reason x=9.0 and x=13.0
# read as unusable below: they are not keys with no free side, they are a
# heavier press of the key next to them. GUI sat there and could not be hit;
# Tab was worse.
#
# CHORDS: a thumb holds ONE of these at a time. The exception is a fat-finger
# pair, of which knuckle+nail is the reliable one -- that is the only way a
# single thumb holds two thumb keys, and it is why alt and MO(2) sit on col 0
# and col 1. Community ranking for the rest: pad is the favourite, nail beats
# knuckle, knuckle is the worst key on the cluster and belongs to a modifier,
# and up strains under heavy use.
THUMB_XY = {
    L_THUMB: {4: (7.4, 7.0), 3: (7.9, 6.0), 2: (8.5, 7.0),
              5: (9.0, 6.0), 0: (9.6, 7.0), 1: (10.1, 6.0)},
    R_THUMB: {0: (11.4, 7.0), 1: (11.9, 6.0), 2: (12.5, 7.0),
              5: (13.0, 6.0), 4: (13.6, 7.0), 3: (14.1, 6.0)},
}

# --- Alpha block placement ---------------------------------------------------
#
# A layout's alpha block is 3 rows x 5 columns, written the way layouts always
# are: outward-in on the left hand, inward-out on the right.
#
#   left:  pinky ring middle index inner      right: inner index middle ring pinky
#
# The Svalboard has four fingers per hand and no inner-index column, so the
# inner column spills onto one inward-facing lateral per finger. That lateral is
# East on the left hand and West on the right.

INNER_LATERAL = {"top": "middle", "home": "index", "bottom": "ring"}


def place(block, hand):
    """Map a 3x5 alpha block onto {(row, col): keycode} for one hand."""
    if hand == "left":
        rows = {"pinky": L_PINKY, "ring": L_RING, "middle": L_MIDDLE, "index": L_INDEX}
        cols = ["pinky", "ring", "middle", "index"]
        inner_col, lateral = 4, E
    else:
        rows = {"pinky": R_PINKY, "ring": R_RING, "middle": R_MIDDLE, "index": R_INDEX}
        cols = ["index", "middle", "ring", "pinky"]
        inner_col, lateral = 0, W

    out = {}
    for i, finger in enumerate(cols):
        col = i if hand == "left" else i + 1
        out[(rows[finger], N)] = block["top"][col]
        out[(rows[finger], C)] = block["home"][col]
        out[(rows[finger], S)] = block["bottom"][col]

    for band, finger in INNER_LATERAL.items():
        out[(rows[finger], lateral)] = block[band][inner_col]

    return out


# --- Layer 0: Hands Down Neu -------------------------------------------------
#
#   w f m p v   / . q " ' z
#   r s n t b   , a e i h j
#   x c l d g   ; u o y k
#
# As it sits on the Glove80, including `;` on the right inner column where
# upstream Hands Down Neu publishes `-`.

HDNEU_LEFT = {
    "top":    ["KC_W", "KC_F", "KC_M", "KC_P", "KC_V"],
    "home":   ["KC_R", "KC_S", "KC_N", "KC_T", "KC_B"],
    "bottom": ["KC_X", "KC_C", "KC_L", "KC_D", "KC_G"],
}

HDNEU_RIGHT = {
    "top":    ["KC_SLASH", "KC_DOT", "KC_Q", "LSFT(KC_QUOTE)", "KC_QUOTE"],
    "home":   ["KC_COMMA", "KC_A", "KC_E", "KC_I", "KC_H"],
    "bottom": ["KC_SCOLON", "KC_U", "KC_O", "KC_Y", "KC_K"],
}

# The Glove80's outer column (z, j) and its bottom row (grave, backslash,
# brackets) have no Svalboard equivalent -- those are columns this keyboard
# doesn't have. They go on the spare laterals, openers left and closers right.
BASE_LATERALS = {
    (R_PINKY, W): "KC_Z",    # inner, x=22.0
    (R_PINKY, E): "KC_J",    # outer, x=24.0
    # Hands Down Neu publishes `-` on the right inner column; this port puts `;`
    # there, following the Glove80. That evicts `-`, and `-` is far too frequent
    # -- kebab-case, `->`, `--flag` -- to sit behind MO(1). It gets a lateral.
    (L_PINKY, E): "KC_MINUS",
    (L_PINKY, W): "KC_TAB",    # outermost key on the left hand, x=0.0

    # Parens likewise: frequent enough in code to be worth a base-layer key
    # rather than MO(1)+key.
    #
    # They were on the left ring's two laterals, which is the wrong pair. The
    # ring laterals are the hardest keys on this board to hit -- that is a
    # verdict from typing on it, and the only kind available: CUP_XY is KLE
    # layout units, so it can say where a key is drawn but nothing about the
    # reach. Nothing frequent goes on a ring lateral.
    #
    # The middles' outer laterals are the strongest free pair instead, and they
    # mirror: West is outward on the left hand, East outward on the right. That
    # also makes `()` alternate hands rather than roll along one.
    (L_MIDDLE, W): "LSFT(KC_9)",
    (R_MIDDLE, E): "LSFT(KC_0)",

    # `:` takes the index's East, the strongest free right-hand lateral. It is
    # the most frequent symbol that still costs two keys: ~9 per 1000 chars of
    # code written here, and that survives context-checking -- 47% of it is
    # `key: value` in yaml/json/nix and 16% slices and ternaries, against only
    # 27% prose colon. It was shift+`;`, so this saves one keystroke, the same
    # win the parens got.
    (R_INDEX, E): "LSFT(KC_SCOLON)",

    # `&` is the opposite case and lands on a ring lateral deliberately: 0.68
    # per 1000 in code, 0.17 in hand-typed text, the rarest thing on the base
    # layer. The ring laterals are the worst keys here, which is exactly what a
    # symbol this rare should be occupying. It stays on the right hand, where
    # Arensito had it.
    (R_RING, E): "LSFT(KC_7)",

    # In Hands Down the left index owns the entire inner column -- v, b and g
    # are all index-finger keys. INNER_LATERAL has to scatter them, because a
    # Svalboard index cup has only two spare directions for three letters, and
    # it spread them one per finger: v to middle, b to index, g to ring.
    #
    # That breaks the finger assignment for the sake of geometric tidiness. Give
    # the index both of the directions it has, so it keeps two of its three
    # letters and only v -- the rarest -- is exiled to the middle finger:
    #
    #   index East = b (from INNER_LATERAL)   index West = g (here)
    #
    (L_INDEX, W): "KC_G",
    # place() still puts g on the ring's East as bottom-inner, so this has to
    # override it explicitly or g ends up on two cups. What takes the slot is
    # `@`, for the same reason `&` took the right ring's East: it is the rarest
    # thing on this layer, 0.17 per 1000 characters of code written here and
    # 0.01 in hand-typed text -- rarer than `&` on both counts, and most of even
    # that 0.17 is `@types/...` imports and `git@github.com`, which are
    # completed rather than typed.
    #
    # By frequency alone it would stay on MO(1). It gets promoted anyway because
    # this costs nothing: the ring laterals are the worst keys on the board and
    # both were empty, so one key here displaces nothing and still beats two.
    # East is the inward lateral of the two, so `@` takes it and the outward one
    # stays free for the next symbol that has to go somewhere.
    (L_RING, E): "LSFT(KC_2)",

    # `~` is that next symbol, and it takes the outward lateral on the same
    # argument. Measured with freq.py: 0.12 per 1000 hand-typed characters and
    # 0.22 in code here -- twice `@` on the hand-typed side, the same order of
    # magnitude on both. If `@` earned a ring lateral, this earned the other one.
    #
    # It suits a bad key in a way a rate alone does not show: 338 occurrences in
    # the hand-typed corpus, ZERO of them consecutive repeats, longest run 1.
    # You hit `~` once at the front of a path and leave. A key you never have to
    # hit twice in a row is the right kind of key to put somewhere awkward.
    #
    # This is also why `` ` `` did NOT take it, despite being the strongest
    # promotion candidate on the board by frequency (8.34 hand-typed, 3.58 in
    # code -- ahead of `:` and `(`, which both got good keys). Backtick comes in
    # pairs around inline code spans, so a ring lateral would be the worst key
    # here hit twice per span. It stays on MO(1) at the left pinky's West, where
    # it is at least a left-right alternation against the right-thumb MO(1).
    # There is no good free key left for it; the honest answer is that it wants
    # one and none exists.
    (L_RING, W): "LSFT(KC_GRAVE)",
}

# Thumb clusters, declared by physical x rather than column index -- the indices
# are not mirrored between hands and reasoning about them directly has already
# produced one wrong answer. THUMB_XY converts.
#
# The KLE draws two interleaved rows, but see THUMB_XY: what those coordinates
# are really showing is six parts of one thumb. x=9.0 and x=13.0 are the
# double-down, the deep press on the down key at x=8.5 and x=12.5 -- which is
# why nothing you press often belongs there. They were found empirically, as
# "the only positions with no free side", years before the mechanism had a name.
#
# Space left / Backspace right matches Glove80 positions 69 and 74; Svalboard's
# own configs ship those mirrored.
#
# GUI used to sit on both x=9.0 and x=13.0, i.e. on both double-downs, which is
# how they were found. There is now one GUI, on the left at x=10.1, matching the
# Glove80 (which has LGUI at 70 and nothing on the right thumb).
#
# The two double-downs get keys you genuinely never need in a hurry. Tab was
# tried at x=9.0 and is emphatically not one of those -- shell completion alone
# makes it constant -- so it moved off the cluster entirely, to the left pinky's
# West lateral. That is x=0.0, the outermost point of the left hand, and the
# closest thing this keyboard has to the Glove80's outer column where Tab
# actually lives (position 22). Delete takes x=9.0 in its place; the Glove80
# also has Delete on a thumb (55). RALT takes x=13.0, likewise from the Glove80
# right thumb (72).
BASE_L_THUMB_X = {
    7.4: "KC_ESCAPE",   # up
    7.9: "KC_SPACE",    # pad -- the favourite key, and the most-pressed thumb
    8.5: "KC_LSHIFT",   # down
    9.0: "KC_DELETE",   # double-down -- nothing time-critical here
    9.6: "KC_LCTRL",    # knuckle
    10.1: "KC_LGUI",    # nail
}

BASE_R_THUMB_X = {
    11.4: "OSM(MOD_LALT)",  # knuckle -- one-shot, see below
    11.9: "MO(2)",      # nail -- chords with alt, see below
    12.5: "MO(1)",      # down
    13.0: "KC_RALT",    # double-down
    13.6: "KC_ENTER",   # up
    14.1: "KC_BSPACE",  # pad
}

# MO(2) is on the nail so that alt can be held with it.
#
# It used to be on `up` at x=13.6, and that made alt+arrow -- so alt+shift+down,
# ctrl+alt+left, the lot -- not awkward but *unpressable*. One thumb holds one
# thumb key; alt is on the knuckle, and no thumb reaches knuckle and up at once.
# Shift, ctrl and GUI escape this by living on the left thumb. Alt cannot follow
# them: the left thumb's six seats are full, and this layout has both layer
# holds and alt on the right, against the usual advice of keeping modifiers on
# one thumb and layer switches on the other.
#
# So MO(2) moved to the one seat that chords with alt -- knuckle+nail, the
# fat-finger pair -- and enter took `up` in exchange. Enter is a tap and pays
# `up`'s strain once; MO(2) is a hold and was paying it for the length of every
# arrow key. Alt, MO(1) and backspace do not move.
#
# That fixes alt+arrow and costs alt+enter, which was the previous occupant of
# knuckle+nail. Hence the second half: alt is a ONE-SHOT.
#
# A one-shot alt sticks to the next key, so it needs no chord at all --
# alt+enter, alt+backspace and alt+arrow all become tap-then-press, and
# alt+backspace has never been available on this layout by any other route
# (backspace is on the pad, and the knuckle pairs only with the nail). Holding
# it is unchanged and still a plain alt hold, so alt+tab still cycles.
#
# One-shot mods are one of the three fixes the Svalboard community names for
# exactly this squeeze; the others are one layer hold per thumb (this layout has
# both on the right) and bottom-row mods (South is alphas here). It must be
# OSM(MOD_LALT) and not OSM(KC_LALT) -- Vial accepts both, but they are
# different keycodes (0x52A8 vs 0x52E3) and the latter reloads as raw hex.


def thumb_row(by_x, row):
    """Turn an {x: keycode} thumb declaration into column order."""
    x_to_col = {xy[0]: col for col, xy in THUMB_XY[row].items()}
    if set(by_x) != set(x_to_col):
        raise SystemExit(
            f"thumb row {row}: declared x positions {sorted(by_x)} do not match "
            f"the hardware's {sorted(x_to_col)}"
        )
    cells = [None] * COLS
    for x, code in by_x.items():
        cells[x_to_col[x]] = code
    return cells


def thumb_col(row, x):
    """Column index of the thumb key at physical x, for sparse overrides."""
    for col, xy in THUMB_XY[row].items():
        if xy[0] == x:
            return col
    raise SystemExit(
        f"thumb row {row}: no key at x={x}, hardware has "
        f"{sorted(xy[0] for xy in THUMB_XY[row].values())}"
    )

# --- Layer 1: Arensito symbols ----------------------------------------------
#
#   { } [ ] @   & _ < > $
#   ; / - 0 :   \ 1 ( ) =
#   6 7 8 9 +   * 2 3 4 5
#
# Ported from the Glove80 `symbols` layer, reached there by a sticky `&sl 3`
# and here by holding MO(1) on the right thumb.

# Seven of Arensito's own cells are holes here. Each of those symbols is already
# reachable in one key elsewhere, and nothing should be on two keys:
#
#   `-`     base layer, left pinky East          -- no MO(1) at all
#   `(` `)` base layer, left/right middle outer  -- no MO(1) at all
#   `:`     base layer, right index East         -- no MO(1) at all
#   `&`     base layer, right ring East          -- no MO(1) at all
#   `@`     base layer, left ring East           -- no MO(1) at all
#   `_`     left thumb's space key               -- see SYMBOL_THUMB_X below
#
# They are KC_NO rather than KC_TRNS on purpose. Transparent would fall through
# to layer 0 and quietly type `n`, `.`, `e`, `i`, `b`, `v` and `/` from the
# middle of the symbol layer; a dead key is the lesser failure.
ARENSITO_LEFT = {
    "top":    ["LSFT(KC_LBRACKET)", "LSFT(KC_RBRACKET)", "KC_LBRACKET", "KC_RBRACKET", "KC_NO"],   # was `@`
    "home":   ["KC_SCOLON", "KC_SLASH", "KC_NO", "KC_0", "KC_NO"],   # were `-` `:`
    "bottom": ["KC_6", "KC_7", "KC_8", "KC_9", "LSFT(KC_EQUAL)"],
}

ARENSITO_RIGHT = {
    "top":    ["KC_NO", "KC_NO", "LSFT(KC_COMMA)", "LSFT(KC_DOT)", "LSFT(KC_4)"],   # were `&` `_`
    "home":   ["KC_BSLASH", "KC_1", "KC_NO", "KC_NO", "KC_EQUAL"],   # were `(` `)`
    "bottom": ["LSFT(KC_8)", "KC_2", "KC_3", "KC_4", "KC_5"],
}

# Arensito uses no lateral on the left pinky, and grave has to live somewhere:
# it is on the Glove80's base layer and `!#%^` stay reachable as shift+digit
# only because this layer carries all ten digits. Grave has no such fallback.
# Arensito's own grid covers most of ASCII, but ! # % ^ | ~ are only reachable
# as shift+digit or shift+backslash -- and since the digits and backslash are
# themselves on this layer, that means holding MO(1) AND shift AND the key.
# Three keys for `!` is not a symbol layer doing its job. They go on the laterals
# Arensito leaves free, paired with their unshifted partners where that helps:
# | beside \. (`~` used to sit beside `` ` `` here for the same reason; it is on
# the base layer now, so the pairing is gone and the slot with it.)
#
# `?` is deliberately absent: it is shift+/ and `/` is on the base layer, so it
# already costs two keys, not three.
SYMBOL_LATERALS = {
    (L_PINKY, W): "KC_GRAVE",
    # `~` was here. It is on the base layer at the left ring's West now, and a
    # promoted symbol is never copied -- leaving it would put it on two keys.
    # KC_NO rather than dropping the entry: dropping it falls through to layer
    # 0's `-` on this lateral, which is a worse failure than a dead key.
    (L_PINKY, E): "KC_NO",
    (L_INDEX, W): "LSFT(KC_1)",        # !  easiest left lateral, most frequent
    (L_MIDDLE, W): "LSFT(KC_3)",       # #
    (L_RING, W): "LSFT(KC_5)",         # %
    (R_INDEX, E): "LSFT(KC_BSLASH)",   # |  beside \ on the index's West
    (R_MIDDLE, E): "LSFT(KC_6)",       # ^
}

# `_` also takes the left thumb's space key. Arensito puts it on the right
# index's North, which is the same hand as MO(1) on the right thumb -- so
# snake_case is a same-hand contortion. On space it is MO(1) right, `_` left,
# the same alternating roll as any other layer-1 symbol. `_` stays on the right
# index too; this is a second seat, not a move.
#
# The cost is that space is not typeable while MO(1) is held. Every other thumb
# stays transparent, so shift, ctrl, enter and backspace all still work on the
# layer. Declared by x like the base thumbs, for the reason in BASE_L_THUMB_X.
SYMBOL_THUMB_X = {
    L_THUMB: {7.9: "LSFT(KC_MINUS)"},
}

# --- Layer 2: navigation and function keys -----------------------------------
#
# Arrows four-across on the right hand, matching Glove80 base-layer positions
# 75-78 (LEFT DOWN UP RIGHT), innermost to outermost. The Svalboard has no spare
# row for them, hence a layer.

NAV = {
    (L_PINKY, N): "KC_F1", (L_PINKY, C): "KC_HOME", (L_PINKY, S): "KC_F9",
    (L_RING, N): "KC_F2", (L_RING, C): "KC_END", (L_RING, S): "KC_F10",
    (L_MIDDLE, N): "KC_F3", (L_MIDDLE, S): "KC_PSCREEN",
    (L_INDEX, N): "KC_F4", (L_INDEX, S): "KC_MPLY",

    (R_INDEX, N): "KC_F5", (R_INDEX, C): "KC_LEFT", (R_INDEX, S): "KC_VOLU",
    (R_MIDDLE, N): "KC_F6", (R_MIDDLE, C): "KC_DOWN", (R_MIDDLE, S): "KC_VOLD",
    (R_RING, N): "KC_F7", (R_RING, C): "KC_UP", (R_RING, S): "KC_F11",
    (R_RING, E): "KC_PGUP",
    (R_PINKY, N): "KC_F8", (R_PINKY, C): "KC_RIGHT", (R_PINKY, S): "KC_F12",
    (R_PINKY, E): "KC_PGDOWN",
}

# --- Layer 15: mouse ---------------------------------------------------------
#
# Svalboard's firmware switches to layer 15 by itself when the pointing device
# moves, and leaves on the first key that isn't on it -- there is deliberately
# no key that reaches this layer. USER06 is SV_RECALIBRATE_POINTER (the custom
# keycode names come from the keyboard's own vial.json).

MOUSE = {
    (L_INDEX, S): "KC_BTN1", (L_MIDDLE, S): "KC_BTN3", (L_RING, S): "KC_BTN2",
    (L_PINKY, C): "USER06",
    (R_INDEX, S): "KC_BTN1", (R_MIDDLE, S): "KC_BTN3", (R_RING, S): "KC_BTN2",
    (R_PINKY, C): "USER06",
}


def blank_layer(fill):
    """A 10x6 layer of `fill`, with column 5 held at -1 on the finger rows."""
    layer = []
    for row in range(ROWS):
        if row in THUMB_ROWS:
            layer.append([fill] * COLS)
        else:
            layer.append([fill] * (COLS - 1) + [-1])
    return layer


def build_layers():
    layers = [blank_layer("KC_NO") for _ in range(LAYERS)]

    # Layer 0 -- alphas, laterals, thumbs.
    base = {}
    base.update(place(HDNEU_LEFT, "left"))
    base.update(place(HDNEU_RIGHT, "right"))
    base.update(BASE_LATERALS)
    for (row, col), code in base.items():
        layers[0][row][col] = code
    layers[0][L_THUMB] = thumb_row(BASE_L_THUMB_X, L_THUMB)
    layers[0][R_THUMB] = thumb_row(BASE_R_THUMB_X, R_THUMB)

    # Layers 1, 2 and 15 fall through to layer 0 wherever they define nothing.
    for index, table in ((1, {}), (2, {}), (15, {})):
        layers[index] = blank_layer("KC_TRNS")

    sym = {}
    sym.update(place(ARENSITO_LEFT, "left"))
    sym.update(place(ARENSITO_RIGHT, "right"))
    sym.update(SYMBOL_LATERALS)
    for (row, col), code in sym.items():
        layers[1][row][col] = code
    for row, by_x in SYMBOL_THUMB_X.items():
        for x, code in by_x.items():
            layers[1][row][thumb_col(row, x)] = code

    # Arensito covers every finger cup except the laterals; anything it leaves
    # alone should fall through rather than repeat layer 0's brackets.
    for (row, col), code in NAV.items():
        layers[2][row][col] = code
    for row in FINGER_ROWS:
        for col in (S, E, C, N, W):
            if (row, col) not in NAV:
                layers[2][row][col] = "KC_NO"

    for (row, col), code in MOUSE.items():
        layers[15][row][col] = code

    return layers


def check_geometry():
    """Verify S/E/C/N/W against the hardware's own coordinates.

    This is the check that matters. Everything else in this file assumes column
    0 is South, 1 East, 2 Center, 3 North, 4 West -- get that wrong and the
    build still emits a structurally perfect .vil with every key in the wrong
    place. CUP_XY comes from the keyboard, so the assertion is against physical
    reality rather than against another config's conventions.
    """
    bad = []
    for row, dirs in CUP_XY.items():
        name = FINGER_NAMES[row]
        cx, cy = dirs[C]
        checks = [
            ("North above Center", dirs[N][1] < cy and dirs[N][0] == cx),
            ("South below Center", dirs[S][1] > cy and dirs[S][0] == cx),
            ("East right of Center", dirs[E][0] > cx and dirs[E][1] == cy),
            ("West left of Center", dirs[W][0] < cx and dirs[W][1] == cy),
        ]
        for label, ok in checks:
            if not ok:
                bad.append(f"  {name}: {label} -- got {dirs}")

    if bad:
        raise SystemExit("geometry check FAILED:\n" + "\n".join(bad))
    return len(CUP_XY) * 4


def check_coverage(layers):
    """Every letter typeable, every digit typeable, nothing silently missing."""
    present = {k for lay in layers for row in lay for k in row if isinstance(k, str)}

    missing = [c for c in "abcdefghijklmnopqrstuvwxyz"
               if f"KC_{c.upper()}" not in present]
    if missing:
        raise SystemExit(f"coverage FAILED -- letters unreachable: {missing}")

    digits = [d for d in "0123456789" if f"KC_{d}" not in present]
    if digits:
        raise SystemExit(f"coverage FAILED -- digits unreachable: {digits}")

    # Grave has no shift-reachable fallback the way !#%^ do; it went missing
    # once already when Arensito's `{` landed on top of it.
    for code in ("KC_GRAVE", "KC_SPACE", "KC_BSPACE", "KC_ENTER", "KC_TAB"):
        if code not in present:
            raise SystemExit(f"coverage FAILED -- {code} is on no layer")

    # Every printable ASCII symbol should cost at most two keys: a direct
    # keycode somewhere, or shift plus a base-layer key. Anything needing
    # MO(1)+shift+key is a three-key combo and counts as missing.
    DIRECT = {
        "!": "LSFT(KC_1)", '"': "LSFT(KC_QUOTE)", "#": "LSFT(KC_3)",
        "$": "LSFT(KC_4)", "%": "LSFT(KC_5)", "&": "LSFT(KC_7)",
        "'": "KC_QUOTE", "(": "LSFT(KC_9)", ")": "LSFT(KC_0)",
        "*": "LSFT(KC_8)", "+": "LSFT(KC_EQUAL)", ",": "KC_COMMA",
        "-": "KC_MINUS", ".": "KC_DOT", "/": "KC_SLASH",
        ":": "LSFT(KC_SCOLON)", ";": "KC_SCOLON", "<": "LSFT(KC_COMMA)",
        "=": "KC_EQUAL", ">": "LSFT(KC_DOT)", "?": "LSFT(KC_SLASH)",
        "@": "LSFT(KC_2)", "[": "KC_LBRACKET", "\\": "KC_BSLASH",
        "]": "KC_RBRACKET", "^": "LSFT(KC_6)", "_": "LSFT(KC_MINUS)",
        "`": "KC_GRAVE", "{": "LSFT(KC_LBRACKET)", "|": "LSFT(KC_BSLASH)",
        "}": "LSFT(KC_RBRACKET)", "~": "LSFT(KC_GRAVE)",
    }
    base_codes = {k for row in layers[0] for k in row if isinstance(k, str)}
    costly = []
    for char, code in sorted(DIRECT.items()):
        if code in present:
            continue
        # shift + a base-layer key is fine; shift + a layer-1 key is not
        if code.startswith("LSFT(") and code[5:-1] in base_codes:
            continue
        costly.append(char)
    if costly:
        raise SystemExit(
            "symbol FAILED -- these cost MO(1)+shift+key: " + " ".join(costly)
        )

    # Moving a key by adding an override leaves the original in place unless it
    # is explicitly blanked -- that is how `g` briefly ended up on two cups.
    # Nothing on the base layer should be reachable from two positions.
    seen = {}
    for row, cells in enumerate(layers[0]):
        for col, code in enumerate(cells):
            if not isinstance(code, str) or code in ("KC_NO", "KC_TRNS"):
                continue
            if code in seen:
                raise SystemExit(
                    f"duplicate FAILED -- {code} is on base layer at "
                    f"{seen[code]} and again at (row {row}, col {col})"
                )
            seen[code] = f"(row {row}, col {col})"

    for index, layer in enumerate(layers):
        if len(layer) != ROWS:
            raise SystemExit(f"layer {index} has {len(layer)} rows, want {ROWS}")
        for row, cells in enumerate(layer):
            if len(cells) != COLS:
                raise SystemExit(f"layer {index} row {row} has {len(cells)} cols")
            if row not in THUMB_ROWS and cells[5] != -1:
                raise SystemExit(
                    f"layer {index} row {row} col 5 is {cells[5]!r}, must be -1 "
                    "on finger rows"
                )
    return len(present)


def main():
    checked = check_geometry()
    print(f"geometry OK: {checked} direction assertions against the hardware")

    layers = build_layers()
    distinct = check_coverage(layers)
    print(f"coverage OK: 26 letters, 10 digits, {distinct} distinct keycodes")

    vil = {
        "version": 1,
        "uid": UID,
        "layout": layers,
        "encoder_layout": [[] for _ in range(LAYERS)],
        "layout_options": -1,
        "macro": [[] for _ in range(50)],
        "vial_protocol": 6,
        "via_protocol": 9,
        "tap_dance": [["KC_NO", "KC_NO", "KC_NO", "KC_NO", 200] for _ in range(50)],
        "combo": [["KC_NO"] * 5 for _ in range(50)],
        "key_override": [
            {
                "trigger": "KC_NO",
                "replacement": "KC_NO",
                "layers": 65535,
                "trigger_mods": 0,
                "negative_mod_mask": 0,
                "suppressed_mods": 0,
                "options": 7,
            }
            for _ in range(10)
        ],
        # Svalboard's QMK settings, as read off the keyboard. Vial writes these
        # back verbatim; they are timings and pointer tuning, not key positions.
        "settings": {
            "1": 0, "2": 50, "3": 0, "4": 175, "5": 5, "6": 5000, "7": 200,
            "8": 0, "9": 150, "10": 60, "11": 8, "12": 5, "13": 7, "14": 10,
            "15": 80, "16": 8, "17": 40, "18": 0, "19": 80, "20": 5, "21": 128,
        },
    }

    OUT.write_text(json.dumps(vil, indent=2) + "\n")
    live = sum(1 for lay in layers for row in lay for k in row
               if isinstance(k, str) and k not in ("KC_NO", "KC_TRNS"))
    print(f"wrote {OUT.name}: {LAYERS} layers, {live} live keys")


if __name__ == "__main__":
    sys.exit(main())
