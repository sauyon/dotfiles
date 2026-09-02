#!/usr/bin/env python3
"""Run jeffzi/svalboard_layout_optimizer over the corpus freq.py collects.

    python3 optimize.py --opt ~/src/svalboard_layout_optimizer          # full run
    python3 optimize.py --opt ... --ngrams-only                         # just build ngrams
    python3 optimize.py --opt ... --evaluate-only                       # re-score, no search

WHY THIS EXISTS
---------------
The optimizer ships corpora of English prose. This layout is not typed by an
average English speaker; it is typed by one person writing agent prompts, shell
commands, Nix and Slack. freq.py already collects that corpus. This is the
bridge: corpus -> per-source ngram directories -> one weighted blend -> search.

WHAT IT REPORTS, AND WHY IT IS A DELTA
--------------------------------------
An optimizer's "optimum" is unfalsifiable against a hand-designed layout: it is
the best point in a space defined by somebody's weights, and the weights are the
argument. So every run evaluates the CURRENT layout under the same config and
the same corpus, and the output is the comparison. A layout that wins by 2% on a
metric nobody has calibrated is not an instruction to re-learn your keyboard.

WHAT MOVES AND WHAT DOES NOT
----------------------------
Thumbs never move: config/keyboard/sval.yml marks all twelve thumb switches
`fixed_keys: true`, so they are not in the layout string at all. That is the
right outcome for the reason keyboards/svalboard/README.md gives -- the thumb
cluster is argued from hardware the optimizer cannot model (one thumb holds one
key; only the knuckle+nail fat-finger pair chords; double-down is a harder press
of down, not a sixth position).

On the fingers, the 26 alphas and the 7 promoted symbols float. The other six
printable layer-0 keys (`,` `.` `/` `;` `'` `"`) are pinned with --fix, because
sval.yml marks every finger key permutable and --fix is the only per-key freeze
that does not mean editing a config whose key costs were fitted to it.

THE CORPUS NEVER ENTERS THIS REPO
---------------------------------
Corpus text and the ngram files derived from it are written under the optimizer
checkout and $CACHE, never here. A 3-gram file is not an aggregate -- it leaks
substrings -- and this repo is public.
"""

import argparse
import json
import pathlib
import re
import shutil
import subprocess
import sys

import build
import cheatsheet
import freq

HERE = pathlib.Path(__file__).parent

# Per-cup order in config/keyboard/sval.yml: north, then the lower-x lateral,
# center, the higher-x lateral, south. East is screen-right on both hands, so
# the lower-x lateral is West on both -- which is the OUTWARD key on the left
# hand and the INWARD one on the right. build.py's own constants say the same
# thing; taking them from there rather than restating them is the point.
CUP_ORDER = (build.N, build.W, build.C, build.E, build.S)

# Left to right as the optimizer walks them: left hand outward-in, then right
# hand inward-out.
CUP_ROWS = (
    build.L_PINKY, build.L_RING, build.L_MIDDLE, build.L_INDEX,
    build.R_INDEX, build.R_MIDDLE, build.R_RING, build.R_PINKY,
)

PLACEHOLDER = "□"

# What floats: the 26 alphas and the 7 symbols the README argues onto layer 0.
PERMUTING = set("abcdefghijklmnopqrstuvwxyz") | set("-:()&@~")

# What stays: the rest of layer 0's printable keys, pinned via --fix. Not
# because they are good placements -- they are simply outside this run's scope,
# and sval.yml marks every finger key permutable, so --fix is the only per-key
# freeze that does not mean editing a config whose key costs were fitted to it.
FIXED = set(",./;\"'")

# How much of a day's typing each corpus stands for. NOT the corpus sizes: those
# are sampling artifacts, and the Discord export proved it. It arrived at 7.4M
# characters -- 339,675 messages over ~14 years, the largest corpus here, bigger
# than the prompt transcripts -- while the Slack sample is 15k. Weighting by
# size would let a decade of chat outvote what Sauyon types now, which is what
# the layout is actually for; weighting it at the 0.05 it had while the export
# was still missing would ignore real personal typing. 0.20 splits that: present
# and substantial, not dominant.
#
#
# `code` is low on purpose, and lower than its 683k characters suggest. Sauyon
# does not write much code by hand any more -- the typing that replaced it is
# in `prompts`, which is why that one carries the largest share. The corpus is
# also the weakest proxy here: it is the text tracked in this repo, which
# includes plenty nobody sat and typed.
#
# These are estimates, they are the softest number in the whole pipeline, and
# they are here as one editable constant so a result can be re-run against a
# different guess -- do that before believing any delta. ngram_merge normalizes
# each component to the first one's total before applying these, so they are
# shares of the blend rather than raw counts.
WEIGHTS = {
    "prompts": 0.38,
    "shell":   0.18,
    "code":    0.08,
    "slack":   0.10,
    "discord": 0.26,
}

PATH_MARKER = re.compile(r"\x00PATH:[^\n]*\n?")

# --name is joined onto the checkout and the result is passed to shutil.rmtree.
# A name containing a separator, or "..", or nothing at all, walks that rmtree
# out of the checkout. This is not a trust boundary -- you are the only one
# passing it -- but the failure mode is deleting the wrong directory, so it is
# worth one regex.
SAFE_NAME = re.compile(r"[A-Za-z0-9][A-Za-z0-9_-]*\Z")


def checked_name(name):
    """The corpus name, or exit if it would escape the optimizer checkout."""
    if not SAFE_NAME.match(name):
        raise SystemExit(
            f"--name {name!r}: use letters, digits, '-' and '_' only. "
            "It is joined onto the checkout and the result gets removed."
        )
    return name


def strip_path_markers(text):
    """Drop iter_code_text's "\\x00PATH:<rel>" file headers.

    They exist so freq.py can bucket the code corpus by extension. Left in,
    every repo path would be counted as text somebody typed.
    """
    return PATH_MARKER.sub("", text)


def layout_string():
    """The current layer 0 as the optimizer's 40-character layout string.

    Read out of build.py's emitted layers rather than transcribed, for the same
    reason cheatsheet.py imports build.py: a second copy of the layout is a
    second thing that can be wrong, and this one would be wrong silently.

    Keys with no single-character glyph -- tab is the only one on layer 0 --
    become the placeholder, which the optimizer treats as a free position.
    """
    layer = build.build_layers()[0]
    out = []
    for row in CUP_ROWS:
        for direction in CUP_ORDER:
            glyph, _ = cheatsheet.label(layer[row][direction])
            out.append(glyph if len(glyph) == 1 else PLACEHOLDER)
    return "".join(out)


# One `["<glyph>"]` entry in base_layout.keys. The stock file has no `"` on any
# key, so a naive [^"]* is enough to FIND them; they are written back with
# json.dumps, which is a YAML subset and quotes correctly.
KEY_ENTRY = re.compile(r'\["[^"]*"\]')

# The finger block: `- [` on its own, up to the first line that is only `]`.
# Entries like `["x"],` cannot end it -- their `]` is not preceded by a newline.
FINGER_BLOCK = re.compile(r"- \[\n.*?\n\s*\]", re.S)


def keyboard_config(text, layout):
    """sval.yml rewritten to carry this layout's glyph inventory.

    optimize_sa refuses any character not in the first level of `base_layout`:

        Invalid keyboard layout: Unsupported characters in provided layout
        (not in first level of `base_layout` ...): '"&()/:;@~'

    Those nine are all on this board and none are in the stock config, whose
    base_layout is the optimizer author's own Hands Down variant. So the
    inventory has to come from here.

    This rewrites the glyphs and NOTHING else. `positions` and `key_costs` in
    that file were fitted to each other -- the costs describe those coordinates,
    not this repo's CUP_XY -- so re-measuring either is out of scope, and the
    thumb block stays as it is because thumbs do not move.
    """
    head, sep, tail = text.partition("base_layout:")
    if not sep:
        raise SystemExit("keyboard config has no base_layout: section")

    keys_at = tail.find("keys:")
    match = FINGER_BLOCK.search(tail, keys_at) if keys_at >= 0 else None
    if not match:
        raise SystemExit("could not find the finger block in base_layout.keys")

    # Count before substituting: running out of glyphs mid-substitution would
    # surface as a StopIteration from inside re, not as the mismatch it is.
    n = len(KEY_ENTRY.findall(match.group()))
    if n != len(layout):
        raise SystemExit(
            f"keyboard config has {n} finger keys but the layout has {len(layout)}"
        )
    glyphs = iter(layout)
    block = KEY_ENTRY.sub(
        lambda _: "[" + json.dumps(next(glyphs), ensure_ascii=False) + "]",
        match.group(),
    )
    return head + sep + tail[:match.start()] + block + tail[match.end():]


def build_ngrams(opt, corpus_dir, name, prefix):
    """Generate <opt>/ngrams/sauyon_<name> from one cached corpus file.

    Returns None when there is nothing to build from -- a cache built before
    this source existed simply has no <name>.txt, and that is main()'s "run
    freq.py first" case, not a traceback.

    The scratch copy is deleted in a finally: it is raw corpus text (shell
    history, work Slack) and the optimizer checkout is not where the docstrings
    promise that lives. A failed cargo run must not leave it there.
    """
    path = corpus_dir / f"{name}.txt"
    if not path.exists():
        return None
    text = path.read_text(encoding="utf-8", errors="replace")
    if name == "code":
        text = strip_path_markers(text)
    if not text.strip():
        return None

    scratch = opt / "temp_corpus"
    scratch.mkdir(exist_ok=True)
    src = scratch / f"{name}.txt"
    src.write_text(text, encoding="utf-8")
    out = opt / "ngrams" / f"{prefix}_{name}"
    try:
        run(opt, ["cargo", "run", "--release", "--bin", "ngrams", "--",
                  str(src), str(out)])
    finally:
        src.unlink(missing_ok=True)
    return out


def run(opt, cmd):
    print(f"  $ {' '.join(cmd[:6])}...", file=sys.stderr)
    return subprocess.run(cmd, cwd=opt, check=True,
                          text=True)


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--opt", required=True, type=pathlib.Path,
                    help="path to a svalboard_layout_optimizer checkout")
    ap.add_argument("--name", default="sauyon", help="corpus name inside the optimizer")
    # Both together would skip the ngram build and then return before
    # evaluating: a silent no-op, so it is not expressible.
    stage = ap.add_mutually_exclusive_group()
    stage.add_argument("--ngrams-only", action="store_true",
                       help="build the blended ngrams and stop")
    stage.add_argument("--evaluate-only", action="store_true",
                       help="re-score existing solutions, run no search")
    args = ap.parse_args()

    # Not `name`: the per-corpus loop below binds that, and a validated value
    # that gets shadowed is worse than no validation -- it reads as checked.
    corpus_name = checked_name(args.name)
    opt = args.opt.expanduser().resolve()
    if not (opt / "Taskfile.yml").exists():
        raise SystemExit(f"{opt} does not look like a svalboard_layout_optimizer checkout")

    current = layout_string()
    print(f"current layout: {current}  ({len(current)} keys)")

    # The stock base_layout is the optimizer author's own Hands Down variant and
    # has none of `"&()/:;@~`, so it rejects this board outright. Derive a config
    # that carries this layout's glyphs and is otherwise byte-identical, and
    # write it into the checkout rather than keeping a second copy in this repo.
    stock = opt / "config" / "keyboard" / "sval.yml"
    derived = opt / "config" / "keyboard" / f"{corpus_name}_sval.yml"
    derived.write_text(keyboard_config(stock.read_text(encoding="utf-8"), current), encoding="utf-8")
    kb = str(derived.relative_to(opt))
    print(f"keyboard config: {kb} (glyphs from build.py, costs from sval.yml)")

    if not args.evaluate_only:
        print("building ngrams per corpus:")
        components = []
        for name, weight in WEIGHTS.items():
            d = build_ngrams(opt, freq.CORPUS_DIR, name, prefix=corpus_name)
            if d is None:
                print(f"  {name}: empty, dropped from the blend", file=sys.stderr)
                continue
            components.append(f"{d}:{weight}")
        if not components:
            raise SystemExit("no corpora -- run freq.py first")

        # Not ignore_errors: a swallowed failure here means ngram_merge writes
        # into a directory still holding the previous run's files, blending
        # against weights nobody chose.
        blend = opt / "ngrams" / corpus_name
        if blend.exists():
            shutil.rmtree(blend)
        print(f"merging with weights {WEIGHTS}")
        run(opt, ["cargo", "run", "--release", "--bin", "ngram_merge", "--",
                  str(blend)] + components)
        run(opt, ["uv", "run", "python", "scripts/ngrams/normalize.py", str(blend)])

    if args.ngrams_only:
        return

    solutions = opt / f"{corpus_name}_optimized_layouts.txt"

    if not args.evaluate_only:
        run(opt, ["cargo", "run", "--release", "--bin", "optimize_sa", "--",
                  "--layout-config", kb,
                  "--eval-parameters", "config/evaluation/sval.yml",
                  "--ngrams", f"ngrams/{corpus_name}",
                  "--start-layouts", current,
                  "--fix", "".join(sorted(FIXED)),
                  "--append-solutions-to", str(solutions)])

    # The delta: score the current layout against whatever the search found,
    # under the same config and the same corpus.
    scored = opt / f"{corpus_name}_compare.txt"
    found = (solutions.read_text(encoding="utf-8").splitlines()
             if solutions.exists() else [])
    lines = [current] + [l for l in found if l.strip() and l.strip() != current]
    scored.write_text("\n".join(lines) + "\n", encoding="utf-8")
    run(opt, ["cargo", "run", "--release", "--bin", "evaluate", "--",
              "--layout-config", kb,
              "--eval-parameters", "config/evaluation/sval.yml",
              "--ngrams", f"ngrams/{corpus_name}",
              "--from-file", str(scored), "--sort"])


if __name__ == "__main__":
    sys.exit(main())
