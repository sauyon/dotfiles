#!/usr/bin/env python3
"""Generate a Svalboard Vial keymap (.vil) for Hands Down Neu + Arensito symbols.

Source of truth for both layers is my Glove80 layout, my.glove80.com layout
e3409150-bb22-49c0-8614-10035f3f6a04, layers `Base` and `symbols`. Its grids
are transcribed into the constants below rather than read at build time, so
this script has no inputs beyond the base .vil.

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
#   col 0   col 1   col 2   col 3   col 4   col 5
#   South   East    Center  North   West    (unused, always -1)

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

# Arensito's `{` lands on the one slot where the stock config kept KC_GRAVE
# (layer 1, left pinky North), and nothing else in 16 layers supplies it. That
# would cost both ` and ~ outright -- unlike !#%^, which stay reachable as
# shift+digit because this layer carries all ten digits. The Glove80 has grave
# on its base layer, so losing it here would be a regression against the source
# layout. Reseat it on the left pinky's outward lateral, which Arensito leaves
# free.
ARENSITO_EXTRA = {
    (L_PINKY, W): "KC_GRAVE",
}

# Blanking the finger cups to KC_TRNS means the laterals Arensito doesn't use
# fall through to layer 0. Note that layer 0 at those eight positions is itself
# inherited from the stock Colemak-DHm config -- see SPARE_LATERALS below.
FINGER_ROWS = [L_INDEX, L_MIDDLE, L_RING, L_PINKY, R_INDEX, R_MIDDLE, R_RING, R_PINKY]

# The eight layer-0 finger slots this script deliberately does NOT write. The
# Glove80 has no equivalent keys -- they belong to columns the Svalboard lacks
# -- so they keep Svalboard's stock assignment: openers on the left hand's
# outward laterals, closers on the right's, plus `-` and Delete on the left
# pinky. Listed here so the inheritance is a decision on the record rather than
# whatever the base file happened to hold.
SPARE_LATERALS = {
    (L_INDEX, W): "(",  (R_INDEX, E): ")",
    (L_MIDDLE, W): "{", (R_MIDDLE, E): "}",
    (L_RING, W): "[",   (R_RING, E): "]",
    (L_PINKY, E): "-",  (L_PINKY, W): "Delete",
}


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


def check_no_orphans(base_layers, out_layers):
    """Fail if overwriting cost a character with no other way to type it.

    Writing 60 positions over a config we didn't author drops whatever was
    underneath, silently. Most drops are harmless -- LSFT(KC_1) is still
    shift+1 as long as KC_1 survives somewhere, and the NONUS_* keys are ISO
    positions this ANSI board has no use for. A bare keycode vanishing outright
    is the case that costs you a character, which is how ` and ~ were lost the
    first time this ran.
    """
    def codes(layers):
        return {k for lay in layers for row in lay for k in row if isinstance(k, str)}

    surviving = codes(out_layers)
    dropped = codes(base_layers) - surviving

    orphaned = []
    for code in sorted(dropped):
        if "NONUS" in code:
            continue  # ISO-only position, absent from this keyboard
        if code.startswith("LSFT(") and code[5:-1] in surviving:
            continue  # still reachable by holding shift
        orphaned.append(code)

    if orphaned:
        raise SystemExit(
            "orphan check FAILED -- these keycodes are gone from all layers "
            "with no shift-reachable source:\n"
            + "\n".join(f"  {c}" for c in orphaned)
        )
    return len(dropped)


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
    sym.update(ARENSITO_EXTRA)
    for (row, col), code in sym.items():
        layers[1][row][col] = code

    orphans = check_no_orphans(json.loads(BASE.read_text())["layout"], layers)
    print(f"orphan check OK: {orphans} keycodes dropped, all shift- or ISO-explained")

    OUT.write_text(json.dumps(vil, indent=2) + "\n")
    print(f"wrote {OUT.name}: {len(base)} base + {len(sym)} symbol positions, "
          f"{len(layers)} layers")


if __name__ == "__main__":
    sys.exit(main())
