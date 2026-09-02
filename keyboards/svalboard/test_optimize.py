#!/usr/bin/env python3
"""Tests for optimize.py's bridge into the layout optimizer.

Run: python3 keyboards/svalboard/test_optimize.py

The thing worth pinning here is the *ordering convention*. The optimizer reads
a layout as one flat 40-character string, and its cup order is not this repo's
cup order: `config/keyboard/sval.yml` lays each cup out as N, W, C, E, S, while
build.py's own direction constants are S, E, C, N, W. Get that mapping wrong and
you get a perfectly well-formed string describing a different keyboard, and the
optimizer will happily report an improvement on it. That is the same failure
build.py's check_geometry() exists to catch, one layer out.

So the expected string below is transcribed from README.md's layer-0 table --
the human-readable spec -- not read back out of build.py. If the two disagree,
that is a finding, not a broken test.
"""

from __future__ import annotations

import string
import unittest

import optimize


# README.md "Layer 0 -- Hands Down Neu", whose columns run W N C S E, re-read
# into the optimizer's N W C E S and concatenated cup by cup in the order
# config/keyboard/sval.yml declares: left pinky->index, then right index->pinky.
#
#   L pinky   tab w r x -     ->  w □ r - x      (tab has no glyph: □)
#   L ring    ~ f s c @       ->  f ~ s @ c
#   L middle  ( m n l v       ->  m ( n v l
#   L index   g p t d b       ->  p g t b d
#   R index   , . a u :       ->  . , a : u
#   R middle  / q e o )       ->  q / e ) o
#   R ring    ; " i y &       ->  " ; i & y
#   R pinky   z ' h k j       ->  ' z h j k
CURRENT = "w□r-x" "f~s@c" "m(nvl" "pgtbd" ".,a:u" "q/e)o" '";i&y' "'zhjk"


class LayoutStringTests(unittest.TestCase):
    def test_matches_the_readme_layer_zero_table(self):
        self.assertEqual(optimize.layout_string(), CURRENT)

    def test_is_forty_finger_switches(self):
        # Thumbs are not in the string at all -- sval.yml marks all twelve
        # thumb keys fixed_keys: true, which is what freezes them.
        self.assertEqual(len(optimize.layout_string()), 40)

    def test_every_letter_appears_exactly_once(self):
        s = optimize.layout_string()
        for c in string.ascii_lowercase:
            self.assertEqual(s.count(c), 1, f"{c!r} appears {s.count(c)} times")

    def test_tab_becomes_a_free_slot(self):
        # tab is a real key with no single-character glyph, so the optimizer
        # cannot carry it. It is offered as a free position instead.
        self.assertEqual(optimize.layout_string()[1], "□")


class PermutingSetTests(unittest.TestCase):
    def test_permuting_is_the_alphas_plus_the_seven_promoted_symbols(self):
        self.assertEqual(
            optimize.PERMUTING,
            set(string.ascii_lowercase) | set("-:()&@~"),
        )

    def test_fixed_is_everything_else_on_the_layer(self):
        # Scope: alphas + promoted symbols float, the rest stays put. These six
        # are passed to optimize_sa as --fix.
        self.assertEqual(optimize.FIXED, set(",./;\"'"))

    def test_the_two_sets_partition_the_layout(self):
        glyphs = set(optimize.layout_string()) - {"□"}
        self.assertEqual(optimize.PERMUTING | optimize.FIXED, glyphs)
        self.assertEqual(optimize.PERMUTING & optimize.FIXED, set())


class CorpusScrubbingTests(unittest.TestCase):
    def test_path_markers_are_stripped_from_the_code_corpus(self):
        # freq.iter_code_text() prefixes each file with "\x00PATH:<rel>\n" so
        # that code_split() can attribute characters to a file type. Feeding
        # those to the ngram generator would count the literal letters of
        # "PATH" and of every path in the repo as things Sauyon typed.
        raw = "\x00PATH:keyboards/svalboard/build.py\nprint(1)\n\x00PATH:a.md\nhi\n"

        self.assertEqual(optimize.strip_path_markers(raw), "print(1)\nhi\n")

    def test_text_without_markers_is_untouched(self):
        self.assertEqual(optimize.strip_path_markers("plain\ntext"), "plain\ntext")


class WeightTests(unittest.TestCase):
    def test_weights_cover_exactly_the_corpora_freq_builds(self):
        # A source added to freq.SOURCES without a weight here would be
        # silently dropped from the blend.
        import freq

        self.assertEqual(set(optimize.WEIGHTS), set(freq.SOURCES))

    def test_weights_sum_to_one(self):
        self.assertAlmostEqual(sum(optimize.WEIGHTS.values()), 1.0)


if __name__ == "__main__":
    unittest.main()
