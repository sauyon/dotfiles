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
#   L pinky   tab w r x "     ->  w □ r " x      (tab has no glyph: □)
#   L ring    )   v s c =     ->  v ) s = c
#   L middle  *   m n f l     ->  m * n l f
#   L index   g   p t d b     ->  p g t b d
#   R index   .   ` a u '     ->  ` . a ' u
#   R middle  -   q e o _     ->  q - e _ o
#   R ring    ,   / i y (     ->  / , i ( y
#   R pinky   z   : h k j     ->  : z h j k
CURRENT = "w□r\"x" "v)s=c" "m*nlf" "pgtbd" "`.a'u" "q-e_o" "/,i(y" ":zhjk"


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
    def test_permuting_is_every_glyph_on_the_layer(self):
        self.assertEqual(
            optimize.permuting(CURRENT),
            set(string.ascii_lowercase) | set("-./'`,*:_\"=)("),
        )

    def test_permuting_follows_the_layout_it_is_given(self):
        # The point of deriving it: promote a symbol in build.py and this
        # follows, rather than going quietly stale against a hardcoded list.
        swapped = CURRENT.replace("`", "~").replace("*", "@")
        self.assertEqual(
            optimize.permuting(swapped),
            set(string.ascii_lowercase) | set("-./'~,@:_\"=)("),
        )

    def test_tab_is_the_one_glyph_that_cannot_move(self):
        # Not a character: it is the placeholder, and optimize_sa treats it as
        # a free position rather than something to permute.
        self.assertNotIn(optimize.PLACEHOLDER, optimize.permuting(CURRENT))

    def test_nothing_is_pinned_by_scope(self):
        # `--fix` used to hold `, . / ; " '` because the run was scoped to
        # "alphas plus the seven promoted symbols" and those six were neither.
        # That was never an argument about placement, and it cost: while they
        # were pinned the search could not consider them at all, so every
        # candidate it returned carried their positions unexamined.
        #
        # There is no scope left to protect. The layer holds 26 alphas, tab and
        # 13 symbols; the alphas and tab stay by Sauyon's rule -- no character
        # is demoted to a layer and no alpha goes on a thumb -- and which 13
        # symbols is now measured rather than listed. Everything on the finger
        # keys is free to move, and the thumbs are frozen by sval.yml itself.
        self.assertEqual(optimize.FIXED, set())

    def test_the_layer_zero_punctuation_can_move(self):
        # `;` is deliberately absent: at 1.14 per 1000 characters it ranks 16th
        # and lost its seat to `` ` ``, `*`, `_` and `=`. It is on MO(1).
        for glyph in ",./\"'":
            self.assertIn(glyph, optimize.permuting(CURRENT), glyph)

    def test_the_two_sets_partition_the_layout(self):
        layout = optimize.layout_string()
        glyphs = set(layout) - {"□"}
        floating = optimize.permuting(layout)
        self.assertEqual(floating | optimize.FIXED, glyphs)
        self.assertEqual(floating & optimize.FIXED, set())


class PromotedSymbolTests(unittest.TestCase):
    """Which symbols earn layer 0 is measured, not listed.

    The seven that are there now were argued one at a time, and three of the
    arguments have expired: `@` and `&` were promoted as "the rarest thing on
    this layer, so a ring lateral costs nothing", and `~` on a count of zero
    consecutive repeats. Against the corpus as it now stands they rank 31st,
    27th and 28th among symbols, behind `` ` ``, `*` and `_`, which are not on
    the layer at all. A hardcoded set cannot notice that; a derived one does.
    """

    # Deliberately not real rates: the point is the selection rule, and pinning
    # it to today's measurements would make this a test of the corpus.
    RATES = {
        "e": 90.0, "t": 80.0, "3": 70.0,   # alphanumerics are not candidates
        " ": 60.0, "\n": 55.0,             # nor is whitespace
        ".": 50.0, ",": 45.0,              # ordinary punctuation IS a candidate
        "`": 9.0, "*": 8.0, "_": 7.0, "=": 6.0, "#": 5.0, "@": 1.0,
    }

    def test_picks_the_most_frequent_symbols_in_rate_order(self):
        self.assertEqual(optimize.promoted_symbols(self.RATES, 3), [".", ",", "`"])

    def test_letters_digits_and_whitespace_are_not_symbols(self):
        picked = optimize.promoted_symbols(self.RATES, 6)
        self.assertEqual(picked, [".", ",", "`", "*", "_", "="])

    def test_anything_pinned_is_never_returned(self):
        # FIXED is empty today, so this guards the rule rather than a case:
        # a pinned symbol already holds a seat --fix will not let the search
        # move, and offering it again would double-count that seat.
        picked = optimize.promoted_symbols(self.RATES, 6)
        self.assertEqual(set(picked) & optimize.FIXED, set())

    def test_asking_for_more_than_there_are_returns_what_there_is(self):
        self.assertEqual(len(optimize.promoted_symbols(self.RATES, 99)), 8)


class BlendedRateTests(unittest.TestCase):
    """One rate per character, weighted the same way the ngrams are.

    freq.rates reports per corpus, which is what the README's tables want.
    Ranking wants a single number, and it has to be blended by WEIGHTS or the
    ranking is of whichever corpus happens to be largest -- which is Discord, a
    decade of chat, and not what the layout is for.
    """

    PER_CORPUS = {
        "prompts": {"a": 10.0, "b": 0.0, "_chars": 100},
        "shell":   {"a": 0.0, "b": 20.0, "_chars": 100},
        "code":    {"a": 0.0, "b": 0.0, "_chars": 0},      # collected nothing
    }

    def test_each_rate_is_weighted_by_its_corpus_share(self):
        out = optimize.blended_rates(self.PER_CORPUS, {"prompts": 0.5, "shell": 0.5, "code": 0.0})
        self.assertAlmostEqual(out["a"], 5.0)
        self.assertAlmostEqual(out["b"], 10.0)

    def test_an_empty_corpus_does_not_dilute_the_blend(self):
        # code has weight but no text. Counting it would scale every rate down
        # by its share, which is the "measured zero" bug freq.py already fixed
        # once: absent is not the same as zero.
        out = optimize.blended_rates(self.PER_CORPUS, {"prompts": 0.4, "shell": 0.4, "code": 0.2})
        self.assertAlmostEqual(out["a"], 5.0)
        self.assertAlmostEqual(out["b"], 10.0)

    def test_the_chars_bookkeeping_key_is_not_a_character(self):
        self.assertNotIn("_chars", optimize.blended_rates(self.PER_CORPUS, optimize.WEIGHTS))


class StartLayoutTests(unittest.TestCase):
    """The search starts from the board carrying a corrected inventory.

    Annealing places every floating glyph itself, so where a symbol starts does
    not matter -- but WHICH symbols are in the string does. It is the only
    channel that tells optimize_sa the inventory exists at all.
    """

    # The layout before the measured inventory landed: `; ~ @ &` held seats that
    # `` ` `` `*` `_` `=` have now, at 1.14/0.18/0.05/0.19 per 1000 characters
    # against 5.52/4.42/4.24/2.11.
    PREVIOUS = "w□r-x" "f~s@c" "m(nvl" "pgtbd" ".,a:u" "q/e)o" '";i&y' "'zhjk"

    # The measured top 13 -- what build.py now emits.
    INVENTORY = ["-", ".", "/", "'", "`", ",", "*", ":", "_", '"', "=", ")", "("]

    def test_symbols_that_keep_their_seat_do_not_move(self):
        out = optimize.start_layout(self.PREVIOUS, self.INVENTORY)
        for i, glyph in ((3, "-"), (11, "("), (20, "."), (23, ":"), (35, "'")):
            self.assertEqual(out[i], glyph, f"index {i}")

    def test_demoted_symbols_are_replaced_in_place(self):
        out = optimize.start_layout(self.PREVIOUS, self.INVENTORY)
        self.assertEqual(
            out,
            "w□r-x" "f`s*c" "m(nvl" "pgtbd" ".,a:u" "q/e)o" '"_i=y' "'zhjk",
        )

    def test_letters_are_never_touched(self):
        out = optimize.start_layout(self.PREVIOUS, self.INVENTORY)
        letters = [(i, c) for i, c in enumerate(self.PREVIOUS) if c.isalpha()]
        self.assertEqual(letters, [(i, out[i]) for i, _ in letters])

    def test_the_layout_build_emits_already_carries_the_inventory(self):
        # The point of the whole exercise: build.py and the measurement agree,
        # so a run starts from the real board rather than a corrected copy of
        # it. If freq.py's ranking ever moves, this is what notices.
        self.assertEqual(optimize.start_layout(CURRENT, self.INVENTORY), CURRENT)

    def test_an_inventory_that_does_not_fit_the_seats_is_refused(self):
        # Silently dropping one would hand optimize_sa a board with a glyph
        # missing, and it would report an improvement on it.
        with self.assertRaises(SystemExit):
            optimize.start_layout(CURRENT, self.INVENTORY[:-1])


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

        self.assertIsNone(optimize.build_ngrams(opt, corpus, "slack", "sauyon"))

    def test_an_empty_corpus_file_is_skipped(self):
        opt, corpus = self.with_dirs()
        (corpus / "discord.txt").write_text("   \n")
        self.no_run()

        self.assertIsNone(optimize.build_ngrams(opt, corpus, "discord", "sauyon"))

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
            optimize.build_ngrams(opt, corpus, "shell", "sauyon")

        leftovers = list((opt / "temp_corpus").glob("*.txt"))
        self.assertEqual(leftovers, [])

    def test_the_code_corpus_is_scrubbed_before_the_ngram_binary_sees_it(self):
        opt, corpus = self.with_dirs()
        (corpus / "code.txt").write_text("\x00PATH:a.py\nreal code")
        calls = self.no_run()

        optimize.build_ngrams(opt, corpus, "code", "sauyon")

        self.assertEqual(len(calls), 1)
        _argv, written = calls[0]
        self.assertEqual(written, "real code")


if __name__ == "__main__":
    unittest.main()
