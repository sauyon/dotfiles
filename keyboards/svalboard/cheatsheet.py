#!/usr/bin/env python3
"""Render a printable cheatsheet for the Svalboard keymap.

Reads the layers straight out of build.py, so this cannot drift from the .vil --
there is no second transcription of the layout here, only a keycode-to-glyph
table and the CSS to put each key where the hardware says it is.

Output is a single self-contained HTML file, laid out for portrait US Letter.
Open cheatsheet.html and print it; the @page rule sets the margins.

    python3 cheatsheet.py
"""

import html
import pathlib
import sys

import build

HERE = pathlib.Path(__file__).parent
OUT = HERE / "cheatsheet.html"

# Page geometry. CSS px are 1/96in by definition, which is what makes the fits
# check at the bottom of this file a real check and not a guess.
PAGE_W_IN = 8.5      # portrait US Letter
PAGE_MARGIN_IN = 0.4  # must match the @page rule in CSS
DPI = 96

# One KLE unit, in CSS px. The board is 24.92 units wide, so this is the knob
# that decides whether the sheet fits the paper; check_fits() enforces it.
UNIT = 29
KEY = 0.92  # key box size, in units -- under 1.0 so boxes don't touch

LAYER_TITLES = {
    0: ("Layer 0", "Hands Down Neu — base"),
    1: ("Layer 1", "Arensito symbols — hold MO(1), right thumb"),
    2: ("Layer 2", "Navigation — hold MO(2), right thumb"),
    15: ("Layer 15", "Mouse — entered automatically when the trackball moves"),
}

# --- Keycode display ---------------------------------------------------------
#
# Every live keycode in the emitted layers must resolve to a glyph here; the
# build aborts otherwise, so adding a key to build.py without labelling it is a
# hard error rather than a blank box on the printout.

UNSHIFTED = {
    "KC_MINUS": "-", "KC_EQUAL": "=", "KC_LBRACKET": "[", "KC_RBRACKET": "]",
    "KC_BSLASH": "\\", "KC_SCOLON": ";", "KC_QUOTE": "'", "KC_GRAVE": "`",
    "KC_COMMA": ",", "KC_DOT": ".", "KC_SLASH": "/",
}

# What each unshifted keycode produces with shift held, on a US layout.
SHIFTED = {
    "KC_1": "!", "KC_2": "@", "KC_3": "#", "KC_4": "$", "KC_5": "%",
    "KC_6": "^", "KC_7": "&", "KC_8": "*", "KC_9": "(", "KC_0": ")",
    "KC_MINUS": "_", "KC_EQUAL": "+", "KC_LBRACKET": "{", "KC_RBRACKET": "}",
    "KC_BSLASH": "|", "KC_SCOLON": ":", "KC_QUOTE": '"', "KC_GRAVE": "~",
    "KC_COMMA": "<", "KC_DOT": ">", "KC_SLASH": "?",
}

NAMED = {
    "KC_TAB": "tab", "KC_SPACE": "space", "KC_ENTER": "enter",
    "KC_BSPACE": "bspc", "KC_DELETE": "del", "KC_ESCAPE": "esc",
    "KC_LSHIFT": "shift", "KC_LCTRL": "ctrl", "KC_LGUI": "gui",
    "KC_LALT": "alt", "KC_RALT": "ralt",
    "KC_HOME": "home", "KC_END": "end", "KC_PGUP": "pgup", "KC_PGDOWN": "pgdn",
    "KC_LEFT": "←", "KC_DOWN": "↓", "KC_UP": "↑",
    "KC_RIGHT": "→",
    "KC_VOLU": "vol+", "KC_VOLD": "vol−", "KC_MPLY": "play",
    "KC_PSCREEN": "prtsc",
    "KC_BTN1": "btn 1", "KC_BTN2": "btn 2", "KC_BTN3": "btn 3",
    "USER06": "recal",
    "MO(1)": "MO(1)", "MO(2)": "MO(2)",
}

# Keys that are drawn but carry no legend.
BLANK = {"KC_NO", "KC_TRNS"}


def label(code):
    """Glyph for a keycode, plus a CSS class describing what kind of key it is."""
    if code.startswith("LSFT(") and code.endswith(")"):
        inner = code[5:-1]
        if inner in SHIFTED:
            return SHIFTED[inner], "sym"
        raise SystemExit(f"cheatsheet: no shifted glyph for {code}")

    if len(code) == 4 and code.startswith("KC_") and code[3].isalpha():
        return code[3].lower(), "alpha"
    if len(code) == 4 and code.startswith("KC_") and code[3].isdigit():
        return code[3], "num"
    if code in UNSHIFTED:
        return UNSHIFTED[code], "sym"
    if code.startswith("KC_F") and code[4:].isdigit():
        return "F" + code[4:], "fn"
    if code in NAMED:
        return NAMED[code], "mod"

    raise SystemExit(f"cheatsheet: no label for keycode {code}")


# --- Geometry ----------------------------------------------------------------


# The hardware puts the thumb clusters at y=6.0/7.0 and the lowest finger key at
# y=3.5, so a faithful plot leaves 2.5 units of blank paper across the middle of
# every diagram. Slide the thumbs up until they clear the fingers by a visible
# margin: the clusters are their own island either way, and nothing about which
# thumb key is which depends on that vertical distance.
THUMB_LIFT = 1.25

# Thumb legends are words ("space", "shift", "MO(1)"), not glyphs, and clip at
# the finger-key width. The cluster has room for it -- same-row thumb keys are
# 1.1 units apart, so 1.05 still leaves a hairline between them.
THUMB_W = 1.05


def positions():
    """{(row, col): (x, y, w, h)} in KLE units, from build.py's hardware readout."""
    xy = {}
    for row, dirs in build.CUP_XY.items():
        for direction, (x, y) in dirs.items():
            xy[(row, direction)] = (x, y, KEY, KEY)
    for row, cols in build.THUMB_XY.items():
        for col, (x, y) in cols.items():
            # Centre the wider box on the switch so the interleave still reads.
            xy[(row, col)] = (x - (THUMB_W - KEY) / 2, y - THUMB_LIFT, THUMB_W, KEY)
    return xy


XY = positions()
MIN_X = min(x for x, _, _, _ in XY.values())
MIN_Y = min(y for _, y, _, _ in XY.values())
MAX_X = max(x + w for x, _, w, _ in XY.values())
MAX_Y = max(y + h for _, y, _, h in XY.values())

BOARD_W = (MAX_X - MIN_X) * UNIT
BOARD_H = (MAX_Y - MIN_Y) * UNIT


def render_board(layer, resolve):
    """One keyboard diagram. `resolve(code, row, col)` -> (text, css class)."""
    out = []
    for (row, col), (x, y, w, h) in sorted(XY.items()):
        code = layer[row][col]
        if not isinstance(code, str):
            continue
        text, kind = resolve(code, row, col)
        if row in build.THUMB_ROWS:
            kind += " thumb"
        out.append(
            f'<div class="k {kind}" style="'
            f"left:{(x - MIN_X) * UNIT:.1f}px;top:{(y - MIN_Y) * UNIT:.1f}px;"
            f'width:{w * UNIT:.1f}px;height:{h * UNIT:.1f}px">'
            f"{html.escape(text)}</div>"
        )
    return "\n".join(out)


def render_layer(index, layers):
    """A titled diagram. Above layer 0, KC_TRNS shows what it falls through to.

    A held layer is only half of what your hands are doing -- shift, ctrl, enter
    and backspace stay live under MO(1), and the transparent keys are the reason
    why. Printing them faint says "still there" where an empty box would say
    "gone".
    """
    base = layers[0]

    def resolve(code, row, col):
        if code == "KC_TRNS":
            under = base[row][col]
            if isinstance(under, str) and under not in BLANK:
                return label(under)[0], "trns"
            return "", "dead"
        if code in BLANK:
            return "", "dead"
        return label(code)

    name, subtitle = LAYER_TITLES[index]
    return f"""
<section class="layer">
  <h2><span class="num">{html.escape(name)}</span> {html.escape(subtitle)}</h2>
  <div class="board" style="width:{BOARD_W:.0f}px;height:{BOARD_H:.0f}px">
{render_board(layers[index], resolve)}
  </div>
</section>"""


CSS = f"""
@page {{ size: letter portrait; margin: {PAGE_MARGIN_IN}in; }}

* {{ box-sizing: border-box; }}

body {{
  margin: 0;
  font: 11px/1.4 -apple-system, "Segoe UI", Inter, Helvetica, Arial, sans-serif;
  color: #111;
  background: #fff;
}}

.page {{ width: {BOARD_W:.0f}px; margin: 0 auto; padding: 8px 0 0; }}

h1 {{
  font-size: 15px;
  font-weight: 650;
  margin: 0 0 1px;
  letter-spacing: -0.01em;
}}

.sub {{ color: #666; margin: 0 0 10px; font-size: 10px; }}

.layer {{ margin: 0 0 9px; break-inside: avoid; }}

h2 {{
  font-size: 10.5px;
  font-weight: 500;
  color: #555;
  margin: 0 0 3px;
  padding-bottom: 2px;
  border-bottom: 1px solid #ddd;
}}

h2 .num {{ font-weight: 700; color: #111; margin-right: 6px; }}

.board {{ position: relative; }}

.k {{
  position: absolute;
  border: 1px solid #b4b4b4;
  border-radius: 4px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 13px;
  font-weight: 500;
  background: #fff;
  overflow: hidden;
}}

/* Glyphs keep the full size; the word-legends shrink to fit their box. */
.k.mod, .k.fn {{ font-size: 8.5px; font-weight: 500; color: #333; background: #f4f4f4; }}
/* Only the word-legends; a lone `_` on the thumb keeps glyph size. */
.k.mod.thumb, .k.trns.thumb {{ font-size: 7.5px; }}
.k.alpha {{ font-weight: 600; }}
.k.sym {{ background: #ededed; }}
.k.num {{ background: #f7f7f7; }}
.k.dead {{ border-style: dashed; border-color: #dcdcdc; background: #fff; }}
.k.trns {{ color: #c2c2c2; border-color: #e6e6e6; font-weight: 400; }}

.notes {{
  display: flex;
  gap: 14px;
  font-size: 9px;
  color: #555;
  border-top: 1px solid #ddd;
  padding-top: 6px;
  margin-top: 2px;
}}

.notes div {{ flex: 1; }}
.notes b {{ color: #111; font-weight: 600; display: block; margin-bottom: 2px; }}
.notes p {{ margin: 0 0 3px; }}

.foot {{
  margin-top: 6px;
  font-size: 8px;
  color: #999;
  text-align: right;
}}

@media print {{
  body {{ -webkit-print-color-adjust: exact; print-color-adjust: exact; }}
}}
"""

NOTES = """
<div class="notes">
  <div>
    <b>Reading the diagram</b>
    <p>Each finger sits in a cup of five switches: North, South, East, West and
    Center (the plain down-press). East is screen-right on both hands.</p>
    <p>Dashed = dead key. Grey glyphs on layers 1&nbsp;and&nbsp;2 fall through to
    layer&nbsp;0.</p>
  </div>
  <div>
    <b>Layer 15 &mdash; mouse</b>
    <p>No key reaches it. The firmware switches over when the pointer moves and
    leaves on the first key that isn't on the layer.</p>
    <p>Buttons 1/3/2 on index/middle/ring South, both hands. Pinky Center
    recalibrates the pointer.</p>
  </div>
  <div>
    <b>Departures from the port</b>
    <p><b style="display:inline">g</b> is on the left index's West so the index
    keeps two of its three inner-column letters; <b style="display:inline">v</b>
    is exiled to the middle finger.</p>
    <p><code>-</code> <code>:</code> <code>(</code> <code>)</code>
    <code>&amp;</code> are promoted to layer&nbsp;0 by measured frequency and are
    holes on layer&nbsp;1, never on two keys.</p>
  </div>
</div>
"""


def check_fits():
    """The board must fit the printable width, or the printer silently shrinks.

    Nothing about a too-wide sheet looks wrong on screen -- the browser viewport
    is whatever size it is. It shows up as a scaled-down or clipped printout,
    after you have already walked to the printer.
    """
    printable = (PAGE_W_IN - 2 * PAGE_MARGIN_IN) * DPI
    if BOARD_W > printable:
        raise SystemExit(
            f"page FAILED -- board is {BOARD_W:.0f}px wide, printable width at "
            f"{PAGE_MARGIN_IN}in margins is {printable:.0f}px. Lower UNIT."
        )
    return printable


def check_labels(layers):
    """Every live keycode must have a glyph. Blank boxes are a build failure."""
    codes = {
        code
        for layer in layers
        for row in layer
        for code in row
        if isinstance(code, str) and code not in BLANK
    }
    for code in sorted(codes):
        label(code)
    return len(codes)


def main():
    build.check_geometry()
    layers = build.build_layers()
    build.check_coverage(layers)

    labelled = check_labels(layers)
    print(f"labels OK: {labelled} distinct keycodes all have a glyph")

    printable = check_fits()
    print(f"page OK: {BOARD_W:.0f}px board in {printable:.0f}px of printable width")

    body = [render_layer(index, layers) for index in (0, 1, 2)]

    doc = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Svalboard — Hands Down Neu + Arensito</title>
<style>{CSS}</style>
</head>
<body>
<div class="page">
  <h1>Svalboard Lightly — Hands Down Neu + Arensito symbols</h1>
  <p class="sub">Generated from <code>build.py</code>. Load
  <code>SvalHandsDownNeu.vil</code> in KeyBard; no reflash.</p>
{"".join(body)}
{NOTES}
  <p class="foot">python3 cheatsheet.py</p>
</div>
</body>
</html>
"""
    OUT.write_text(doc)
    print(f"wrote {OUT.name}: 3 layer diagrams, {len(XY)} keys each")


if __name__ == "__main__":
    sys.exit(main())
