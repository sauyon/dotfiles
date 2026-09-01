#!/usr/bin/env python3
"""Generate a Svalboard Vial keymap (.vil) for Hands Down Neu + Arensito symbols.

Source of truth for both layers is my Glove80 layout, exported from
my.glove80.com as glove80-hdneu.json (see README.md). This script re-expresses
that layout on the Svalboard's finger-cup geometry.

The base .vil is Svalboard's own SvalCOLEMAKDHM.vil, which supplies the thumb
clusters, the nav/F-key layer, and all the Vial metadata we don't want to
hand-author. We overwrite only the 40 finger-cup positions on layers 0 and 1.

Run `python3 build.py` to regenerate SvalHandsDownNeu.vil. The build refuses to
emit anything unless the self-test passes -- see check_mapping().
"""

import json
import pathlib
import sys

HERE = pathlib.Path(__file__).parent
BASE = HERE / "SvalCOLEMAKDHM.vil"
OUT = HERE / "SvalHandsDownNeu.vil"

# --- Svalboard .vil geometry -------------------------------------------------
#
# layout[layer] is 10 rows x 6 columns.
#
#   row 0 = left thumb cluster      row 5 = right thumb cluster
#   rows 1-4 = left  index, middle, ring, pinky
#   rows 6-9 = right index, middle, ring, pinky
#
# Within a finger row the columns are the five switches of that finger's cup,
# in absolute directions (East is always screen-right, on both hands):
#
#   [South, East, Center, North, unused]
#          col 0   col 1   col 2   col 3   col 4   col 5
#          South   East    Center  North   West    (unused, always -1)

S, E, C, N, W = 0, 1, 2, 3, 4

L_INDEX, L_MIDDLE, L_RING, L_PINKY = 1, 2, 3, 4
R_INDEX, R_MIDDLE, R_RING, R_PINKY = 6, 7, 8, 9

# A layout's alpha block is 3 rows x 5 columns. Column order runs outward-in on
# the left hand and inward-out on the right, matching how layouts are written:
#
#   left:  pinky ring middle index inner      right: inner index middle ring pinky
#
# The Svalboard has no dedicated inner-index column -- only four fingers. The
# stock Colemak-DHm config resolves this by spilling the inner column onto the
# inward-facing lateral of three different fingers:
#
#   inner-top -> middle    inner-home -> index    inner-bottom -> ring
#
# That inward lateral is East on the left hand and West on the right. We keep
# this convention so muscle memory transfers from Svalboard's own configs.

INNER_LATERAL = {"top": "middle", "home": "index", "bottom": "ring"}


def place(block, hand):
    """Map a 3x5 alpha block onto (row, col) -> keycode for one hand.

    block is {"top": [...5], "home": [...5], "bottom": [...5]} in the column
    order described above. Returns a dict suitable for splatting into a layer.
    """
    top, home, bottom = block["top"], block["home"], block["bottom"]

    if hand == "left":
        rows = {"pinky": L_PINKY, "ring": L_RING, "middle": L_MIDDLE, "index": L_INDEX}
        # left block columns, left to right
        cols = ["pinky", "ring", "middle", "index"]
        inner_col = 4
        lateral = E
    else:
        rows = {"pinky": R_PINKY, "ring": R_RING, "middle": R_MIDDLE, "index": R_INDEX}
        # right block columns run inner-first, so the finger columns are 1..4
        cols = ["index", "middle", "ring", "pinky"]
        inner_col = 0
        lateral = W

    out = {}
    for i, finger in enumerate(cols):
        col = i if hand == "left" else i + 1
        row = rows[finger]
        out[(row, N)] = top[col]
        out[(row, C)] = home[col]
        out[(row, S)] = bottom[col]

    for band, finger in INNER_LATERAL.items():
        out[(rows[finger], lateral)] = block[band][inner_col]

    return out


# --- Layer 0: Hands Down Neu -------------------------------------------------
#
#   w f m p v   / . q " ' z
#   r s n t b   , a e i h j
#   x c l d g   ; u o y k
#
# Exactly as it sits on the Glove80 today, including `;` on the right inner
# column (upstream Hands Down Neu publishes `-` there) and z/j on the outer
# column. The Svalboard has no outer column, so z and j go on the spare pinky
# laterals.

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

# Glove80 outer column, rehomed onto spare Svalboard laterals. TAB/ESC/LSHIFT
# already live on the Svalboard's left thumb cluster, so only z and j need seats.
HDNEU_EXTRA = {
    (R_PINKY, E): "KC_Z",
    (R_PINKY, W): "KC_J",
}

# --- Layer 1: Arensito symbols ----------------------------------------------
#
#   { } [ ] @   & _ < > $
#   ; / - 0 :   \ 1 ( ) =
#   6 7 8 9 +   * 2 3 4 5
#
# Ported verbatim from the Glove80 "symbols" layer (reached there via &sl 3;
# here it is the MO(1) hold on the right thumb).

ARENSITO_LEFT = {
    "top":    ["LSFT(KC_LBRACKET)", "LSFT(KC_RBRACKET)", "KC_LBRACKET", "KC_RBRACKET", "LSFT(KC_2)"],
    "home":   ["KC_SCOLON", "KC_SLASH", "KC_MINUS", "KC_0", "LSFT(KC_SCOLON)"],
    "bottom": ["KC_6", "KC_7", "KC_8", "KC_9", "LSFT(KC_EQUAL)"],
}

ARENSITO_RIGHT = {
    "top":    ["LSFT(KC_7)", "LSFT(KC_MINUS)", "LSFT(KC_COMMA)", "LSFT(KC_DOT)", "LSFT(KC_4)"],
    "home":   ["KC_BSLASH", "KC_1", "LSFT(KC_9)", "LSFT(KC_0)", "KC_EQUAL"],
    "bottom": ["LSFT(KC_8)", "KC_2", "KC_3", "KC_4", "KC_5"],
}

# Spare laterals on the symbol layer fall through to the base layer rather than
# keeping Colemak's leftovers.
FINGER_ROWS = [L_INDEX, L_MIDDLE, L_RING, L_PINKY, R_INDEX, R_MIDDLE, R_RING, R_PINKY]


# --- Self-test ---------------------------------------------------------------
#
# place() encodes a guess about Svalboard's matrix order that, if wrong, would
# produce a plausible-looking but scrambled keymap. So we run the stock
# Colemak-DHm block back through place() and require it to reproduce the
# shipped SvalCOLEMAKDHM.vil alpha positions exactly.

COLEMAK_LEFT = {
    "top":    ["KC_Q", "KC_W", "KC_F", "KC_P", "KC_B"],
    "home":   ["KC_A", "KC_R", "KC_S", "KC_T", "KC_G"],
    "bottom": ["KC_Z", "KC_X", "KC_C", "KC_D", "KC_V"],
}

COLEMAK_RIGHT = {
    "top":    ["KC_J", "KC_L", "KC_U", "KC_Y", "KC_SCOLON"],
    "home":   ["KC_M", "KC_N", "KC_E", "KC_I", "KC_O"],
    "bottom": ["KC_K", "KC_H", "KC_COMMA", "KC_DOT", "KC_SLASH"],
}


def check_mapping(base_layer0):
    """Reproduce Colemak-DHm through place(); compare against the shipped .vil."""
    rebuilt = {}
    rebuilt.update(place(COLEMAK_LEFT, "left"))
    rebuilt.update(place(COLEMAK_RIGHT, "right"))

    bad = []
    for (row, col), want in sorted(rebuilt.items()):
        got = base_layer0[row][col]
        if got != want:
            bad.append(f"  r{row}c{col}: base has {got}, mapping produced {want}")

    if bad:
        raise SystemExit(
            "self-test FAILED -- place() does not reproduce Colemak-DHm:\n"
            + "\n".join(bad)
        )
    return len(rebuilt)


def main():
    if not BASE.exists():
        raise SystemExit(
            f"missing {BASE.name}. Fetch it with:\n"
            "  gh api repos/svalboard/svalboard-configs/contents/SvalCOLEMAKDHM.vil "
            f"--jq .content | base64 -d > {BASE}"
        )

    vil = json.loads(BASE.read_text())
    layers = vil["layout"]

    checked = check_mapping(layers[0])
    print(f"self-test OK: {checked} Colemak-DHm positions reproduced from place()")

    base = {}
    base.update(place(HDNEU_LEFT, "left"))
    base.update(place(HDNEU_RIGHT, "right"))
    base.update(HDNEU_EXTRA)
    for (row, col), code in base.items():
        layers[0][row][col] = code

    # Blank the finger cups on layer 1 so Colemak's symbol layer can't show
    # through on positions Arensito doesn't use.
    for row in FINGER_ROWS:
        for col in (S, E, C, N, W):
            layers[1][row][col] = "KC_TRNS"

    sym = {}
    sym.update(place(ARENSITO_LEFT, "left"))
    sym.update(place(ARENSITO_RIGHT, "right"))
    for (row, col), code in sym.items():
        layers[1][row][col] = code

    OUT.write_text(json.dumps(vil, indent=2) + "\n")
    print(f"wrote {OUT.name}: {len(base)} base + {len(sym)} symbol positions, "
          f"{len(layers)} layers")


if __name__ == "__main__":
    sys.exit(main())
