#!/usr/bin/env python3
"""Does the Svalboard still type like the Glove80?

Run: python3 keyboards/svalboard/test_glove80.py

Sauyon types on a Glove80 at home and a Svalboard otherwise, both running Hands
Down Neu. A key that differs between them is not a one-time relearning cost, it
is a permanent context-switch cost paid on every swap -- so divergence is a
thing to decide deliberately, not to discover later.

The SHARED CORE is the 24 cells that mean the same thing on both boards: three
rows on each of four fingers on each hand, with the Glove80's top/home/bottom
mapping to the Svalboard's North/Center/South. Within it the finger assignments
are identical, which is why same-finger bigrams and scissors carry across.

Outside the core nothing is comparable and nothing is checked. The Glove80's
inner column (`v` `b` `g`), its outer column (`z` `j`), its number row (which is
where `-` lives) and its bottom row have no Svalboard equivalent -- those keys
were relocated by the port itself, and Sauyon has no muscle memory tying them
down. The Svalboard's laterals and thumb cluster have no Glove80 equivalent
either.

`keyboards/glove80/layout.json` is the export from my.glove80.com, saved by hand
because there is no public API behind those URLs. build.py transcribes its grids
into constants; this is what stops the transcription and the export drifting
apart silently, which is exactly what happened in 475c84f.
"""

from __future__ import annotations

import json
import pathlib
import unittest

import build

GLOVE80 = pathlib.Path(__file__).parent.parent / "glove80" / "layout.json"

# The layout build.py says it was ported from. A different export is a different
# keyboard, and every position below would be read off the wrong grid.
SOURCE_UUID = "e3409150-bb22-49c0-8614-10035f3f6a04"

# Glove80 base-layer key positions: (hand, finger) -> (top, home, bottom).
# Read off the export, not guessed: the left hand's five columns are pinky, ring,
# middle, index, inner at 23-27 / 35-39 / 47-51, and the right hand's six are
# inner, index, middle, ring, pinky, outer at 28-33 / 40-45, with the bottom row
# one shorter at 58-62 because the outer column stops.
GLOVE80_CORE = {
    ("L", "pinky"):  (23, 35, 47),
    ("L", "ring"):   (24, 36, 48),
    ("L", "middle"): (25, 37, 49),
    ("L", "index"):  (26, 38, 50),
    ("R", "index"):  (29, 41, 59),
    ("R", "middle"): (30, 42, 60),
    ("R", "ring"):   (31, 43, 61),
    ("R", "pinky"):  (32, 44, 62),
}

SVAL_CUP = {
    ("L", "pinky"):  build.L_PINKY,
    ("L", "ring"):   build.L_RING,
    ("L", "middle"): build.L_MIDDLE,
    ("L", "index"):  build.L_INDEX,
    ("R", "index"):  build.R_INDEX,
    ("R", "middle"): build.R_MIDDLE,
    ("R", "ring"):   build.R_RING,
    ("R", "pinky"):  build.R_PINKY,
}

ROWS = (("top", build.N), ("home", build.C), ("bottom", build.S))

# Cells that deliberately differ, with the Glove80 keycode and the Svalboard one.
# Anything NOT listed here is a regression: a key Sauyon would have to type
# differently depending on which keyboard he is sitting at, introduced by a
# change that did not consider the other board.
#
# All five arrived in 475c84f, which argued entirely from Svalboard geometry and
# does not mention the Glove80:
#   - `f`/`v` and `l`/`f` are two thirds of the f/v/l rotation. It was justified
#     as taking Character Constraints to zero, but the dominant term it removes
#     is the `pl` vertical scissor -- and `pl` is a scissor on the Glove80 too,
#     so this buys comfort on one board by diverging from a property of Hands
#     Down Neu itself.
#   - `.`, `"` and `'` are punctuation the symbol search moved. Sauyon knows
#     these positions; they were relocated on frequency grounds alone.
EXPECTED_DIVERGENCE = {
    ("L", "ring",   "top"):    ("F",       "KC_V"),
    ("L", "middle", "bottom"): ("L",       "KC_F"),
    ("R", "index",  "top"):    ("DOT",     "KC_GRAVE"),
    ("R", "ring",   "top"):    ("LS(SQT)", "KC_SLASH"),
    ("R", "pinky",  "top"):    ("SQT",     "LSFT(KC_SCOLON)"),
}


def keycode(entry: dict) -> str:
    """One Glove80 behaviour as a bare keycode name.

    Only `&kp` carries a keycode; `&trans`, `&none`, `&magic` and friends are
    returned as their behaviour name so a mismatch reads as what it is rather
    than crashing on a missing `params`.
    """
    if entry.get("value") != "&kp":
        return entry.get("value", "?")
    param = entry["params"][0]
    name = param["value"]
    if name == "LS":                       # shifted: LS(SQT) is `"`
        return f"LS({param['params'][0]['value']})"
    return name


class Glove80ParityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.base = json.loads(GLOVE80.read_text(encoding="utf-8"))
        cls.sval = build.build_layers()[0]

    def test_the_export_is_the_layout_build_py_was_ported_from(self):
        self.assertEqual(self.base["uuid"], SOURCE_UUID)

    def test_the_export_still_has_the_shape_the_positions_assume(self):
        # Position indices are meaningless if the export's geometry changed.
        self.assertEqual([len(layer) for layer in self.base["layers"]], [80] * 7)

    def test_the_shared_core_matches_except_where_recorded(self):
        diverged = {}
        for (hand, finger), positions in GLOVE80_CORE.items():
            for (row_name, col), pos in zip(ROWS, positions):
                glove = keycode(self.base["layers"][0][pos])
                sval = self.sval[SVAL_CUP[(hand, finger)]][col]
                # The export writes bare names; build.py writes QMK keycodes.
                if sval != glove and sval != f"KC_{glove}":
                    diverged[(hand, finger, row_name)] = (glove, sval)

        self.assertEqual(
            diverged, EXPECTED_DIVERGENCE,
            "the Svalboard's shared core drifted from the Glove80. Either "
            "apply the same change to keyboards/glove80/layout.json, or record "
            "it in EXPECTED_DIVERGENCE with the reason it is worth typing two "
            "different ways.",
        )

    def test_every_home_row_cell_is_identical(self):
        # Stated separately because it is the strongest claim available: the
        # eight home positions, where the hands rest, mean the same thing on
        # both boards. Any divergence here is worse than one on a top row.
        for (hand, finger), positions in GLOVE80_CORE.items():
            glove = keycode(self.base["layers"][0][positions[1]])
            sval = self.sval[SVAL_CUP[(hand, finger)]][build.C]
            self.assertEqual(sval, f"KC_{glove}", f"{hand} {finger} home")


if __name__ == "__main__":
    unittest.main(verbosity=2)
