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


class CorpusNameTests(unittest.TestCase):
    def test_ordinary_names_pass_through(self):
        self.assertEqual(optimize.checked_name("sauyon"), "sauyon")
        self.assertEqual(optimize.checked_name("sauyon_2026-09"), "sauyon_2026-09")

    def test_names_that_escape_the_checkout_are_refused(self):
        # --name is joined onto the checkout and the result is handed to
        # shutil.rmtree. "/" or ".." would delete outside it. Nobody is
        # attacking anyone here; it is a footgun on your own machine, and the
        # blast radius is rm -rf.
        for bad in ("/", "..", "../../x", "a/b", "", "."):
            with self.subTest(bad=bad), self.assertRaises(SystemExit):
                optimize.checked_name(bad)


SVAL_YML = '''\
keyboard:
  key_costs:
    - [
      99, 6, 3, 5, 4,
    ]

base_layout:
  placeholder: "□"
  keys:
    # fingers
    - [
      # left pinky
             ["x"],
      ["□"], ["r"], ["□"],
             ["w"],
    ]

    # thumbs
    - [
      ["□"], ["□"], ["□"],
    ]

  fixed_keys:
    - [
      false,
    ]
'''


class KeyboardConfigTests(unittest.TestCase):
    """optimize_sa refuses any glyph absent from base_layout's first level.

    The stock config carries the author's own Hands Down variant, which has no
    `"`, `&`, `(`, `)`, `/`, `:`, `;`, `@` or `~` -- so running Sauyon's layer 0
    against it dies with "Unsupported characters in provided layout". The fix is
    a derived config carrying this layout's glyph inventory. It must change the
    inventory and NOTHING else: `positions` and `key_costs` in that file were
    fitted to each other, and re-measuring them is explicitly out of scope.
    """

    def test_finger_glyphs_are_replaced_in_order(self):
        # Assert the whole rewritten block, not three fragments: north/south
        # transposed would satisfy `"a" appears` and `"e" appears` while
        # describing a different keyboard, which is the exact failure the
        # module docstring warns about.
        out = optimize.keyboard_config(SVAL_YML, "abcde")

        self.assertIn(
            '- [\n'
            '      # left pinky\n'
            '             ["a"],\n'
            '      ["b"], ["c"], ["d"],\n'
            '             ["e"],\n'
            '    ]',
            out,
        )

    def test_key_costs_are_untouched(self):
        out = optimize.keyboard_config(SVAL_YML, "abcde")

        self.assertIn("99, 6, 3, 5, 4,", out)
        self.assertEqual(out.partition("base_layout:")[0],
                         SVAL_YML.partition("base_layout:")[0])

    def test_the_thumb_block_is_untouched(self):
        # Thumbs are fixed_keys and are not in the layout string; rewriting them
        # would be the one thing this run is forbidden to do.
        out = optimize.keyboard_config(SVAL_YML, "abcde")

        self.assertIn('["□"], ["□"], ["□"],', out)

    def test_a_layout_of_the_wrong_length_is_refused(self):
        with self.assertRaises(SystemExit):
            optimize.keyboard_config(SVAL_YML, "abc")

    def test_quote_glyphs_are_emitted_as_valid_yaml(self):
        out = optimize.keyboard_config(SVAL_YML, 'a"c\'e')

        self.assertIn('["\\""]', out)
        self.assertIn('["\'"]', out)

    # NOT tested here: that the stock sval.yml really has 40 finger entries in
    # this shape, and that CUP_ORDER matches the optimizer's own positional
    # order. Both live in an ephemeral checkout of someone else's repo, so a
    # test asserting them would be a test of whether that checkout is present.
    # The check that does cover it is the optimizer's own renderer: it draws
    # layer 0 from the string this module hands it, and that drawing matches
    # README.md's table key for key. Run optimize.py --evaluate-only and look.


class BuildNgramsTests(unittest.TestCase):
    def with_dirs(self):
        import tempfile
        from pathlib import Path

        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root = Path(tmp.name)
        (root / "opt").mkdir()
        (root / "corpus").mkdir()
        return root / "opt", root / "corpus"

    def no_run(self):
        """Replace the subprocess wrapper.

        Records (argv, contents of the scratch file it was pointed at) --
        read inside the call, because build_ngrams deletes the scratch file
        as soon as the command returns.
        """
        from pathlib import Path

        calls = []
        old = optimize.run

        def record(opt, cmd, capture=False):
            calls.append((cmd, Path(cmd[-2]).read_text()))

        optimize.run = record
        self.addCleanup(lambda: setattr(optimize, "run", old))
        return calls

    def failing_run(self):
        import subprocess

        old = optimize.run

        def boom(opt, cmd, capture=False):
            raise subprocess.CalledProcessError(1, cmd)

        optimize.run = boom
        self.addCleanup(lambda: setattr(optimize, "run", old))

    def test_a_missing_corpus_file_is_skipped_not_a_traceback(self):
        # An existing $CACHE built before slack/discord existed has no
        # slack.txt. main()'s "run freq.py first" guard is the intended
        # message; a bare FileNotFoundError from here would preempt it, and
        # this is the most likely first run of the new script.
        opt, corpus = self.with_dirs()
        self.no_run()

        self.assertIsNone(optimize.build_ngrams(opt, corpus, "slack"))

    def test_an_empty_corpus_file_is_skipped(self):
        opt, corpus = self.with_dirs()
        (corpus / "discord.txt").write_text("   \n")
        self.no_run()

        self.assertIsNone(optimize.build_ngrams(opt, corpus, "discord"))

    def test_the_scratch_copy_is_removed_even_when_the_build_fails(self):
        # build_ngrams copies raw corpus text into the optimizer checkout so
        # the ngram binary can read it. That text is work Slack and shell
        # history. If cargo fails, it must not be left behind in an unrelated
        # repo -- $CACHE is the only place the docstrings promise it lives.
        opt, corpus = self.with_dirs()
        (corpus / "shell.txt").write_text("some history")
        self.failing_run()

        import subprocess

        with self.assertRaises(subprocess.CalledProcessError):
            optimize.build_ngrams(opt, corpus, "shell")

        leftovers = list((opt / "temp_corpus").glob("*.txt"))
        self.assertEqual(leftovers, [])

    def test_the_code_corpus_is_scrubbed_before_the_ngram_binary_sees_it(self):
        opt, corpus = self.with_dirs()
        (corpus / "code.txt").write_text("\x00PATH:a.py\nreal code")
        calls = self.no_run()

        optimize.build_ngrams(opt, corpus, "code")

        self.assertEqual(len(calls), 1)
        _argv, written = calls[0]
        self.assertEqual(written, "real code")


if __name__ == "__main__":
    unittest.main()
