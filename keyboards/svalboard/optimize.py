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
# are sampling artifacts (the prompt transcripts are ~430x the Slack sample and
# nobody types in that ratio). These are estimates, they are the softest number
# in the whole pipeline, and they are here as one editable constant so that a
# result can be re-run against a different guess. ngram_merge normalizes each
# component to the first one's total before applying these, so they are shares
# of the blend rather than raw counts.
WEIGHTS = {
    "prompts": 0.40,
    "shell":   0.20,
    "code":    0.20,
    "slack":   0.15,
    "discord": 0.05,
}

PATH_MARKER = re.compile(r"\x00PATH:[^\n]*\n?")


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


def build_ngrams(opt, corpus_dir, name):
    """Generate <opt>/ngrams/<name> from one cached corpus file."""
    text = (corpus_dir / f"{name}.txt").read_text(errors="replace")
    if name == "code":
        text = strip_path_markers(text)
    if not text.strip():
        return None
    scratch = opt / "temp_corpus"
    scratch.mkdir(exist_ok=True)
    src = scratch / f"{name}.txt"
    src.write_text(text)
    out = opt / "ngrams" / f"sauyon_{name}"
    run(opt, ["cargo", "run", "--release", "--bin", "ngrams", "--",
              str(src), str(out)])
    src.unlink()
    return out


def run(opt, cmd, capture=False):
    print(f"  $ {' '.join(cmd[:6])}...", file=sys.stderr)
    return subprocess.run(cmd, cwd=opt, check=True,
                          capture_output=capture, text=True)


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--opt", required=True, type=pathlib.Path,
                    help="path to a svalboard_layout_optimizer checkout")
    ap.add_argument("--name", default="sauyon", help="corpus name inside the optimizer")
    ap.add_argument("--ngrams-only", action="store_true")
    ap.add_argument("--evaluate-only", action="store_true")
    args = ap.parse_args()

    opt = args.opt.expanduser().resolve()
    if not (opt / "Taskfile.yml").exists():
        raise SystemExit(f"{opt} does not look like a svalboard_layout_optimizer checkout")

    current = layout_string()
    print(f"current layout: {current}  ({len(current)} keys)")

    if not args.evaluate_only:
        print("building ngrams per corpus:")
        components = []
        for name, weight in WEIGHTS.items():
            d = build_ngrams(opt, freq.CORPUS_DIR, name)
            if d is None:
                print(f"  {name}: empty, dropped from the blend", file=sys.stderr)
                continue
            components.append(f"{d}:{weight}")
        if not components:
            raise SystemExit("no corpora -- run freq.py first")

        blend = opt / "ngrams" / args.name
        shutil.rmtree(blend, ignore_errors=True)
        print(f"merging with weights {WEIGHTS}")
        run(opt, ["cargo", "run", "--release", "--bin", "ngram_merge", "--",
                  str(blend)] + components)
        run(opt, ["uv", "run", "python", "scripts/ngrams/normalize.py", str(blend)])

    if args.ngrams_only:
        return

    layouts = opt / f"{args.name}_layouts.txt"
    solutions = opt / f"{args.name}_optimized_layouts.txt"

    if not args.evaluate_only:
        layouts.write_text(current + "\n")
        run(opt, ["cargo", "run", "--release", "--bin", "optimize_sa", "--",
                  "--layout-config", "config/keyboard/sval.yml",
                  "--eval-parameters", "config/evaluation/sval.yml",
                  "--ngrams", f"ngrams/{args.name}",
                  "--start-layouts", current,
                  "--fix", "".join(sorted(FIXED)),
                  "--append-solutions-to", str(solutions)])

    # The delta: score the current layout against whatever the search found,
    # under the same config and the same corpus.
    scored = opt / f"{args.name}_compare.txt"
    found = solutions.read_text().splitlines() if solutions.exists() else []
    lines = [current] + [l for l in found if l.strip() and l.strip() != current]
    scored.write_text("\n".join(lines) + "\n")
    run(opt, ["cargo", "run", "--release", "--bin", "evaluate", "--",
              "--layout-config", "config/keyboard/sval.yml",
              "--eval-parameters", "config/evaluation/sval.yml",
              "--ngrams", f"ngrams/{args.name}",
              "--from-file", str(scored), "--sort"])


if __name__ == "__main__":
    sys.exit(main())
