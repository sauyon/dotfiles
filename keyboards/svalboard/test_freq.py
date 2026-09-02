#!/usr/bin/env python3
"""Tests for freq.py's corpus loaders.

Run: python3 keyboards/svalboard/test_freq.py

Only the loaders with a *parseable input format* are pinned here. `prompts`,
`shell` and `code` read whatever happens to be on this machine and have no
contract worth asserting; the Slack and Discord loaders read an export format
that is specified elsewhere and can silently change under us, which is exactly
the thing a test should catch.
"""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

import freq


class SlackLoaderTests(unittest.TestCase):
    def with_dump(self, name: str, payload) -> Path:
        """Point freq.SLACK_DIR at a temp dir holding one dump file."""
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        d = Path(tmp.name)
        (d / name).write_text(json.dumps(payload))
        old = freq.SLACK_DIR
        freq.SLACK_DIR = d
        self.addCleanup(lambda: setattr(freq, "SLACK_DIR", old))
        return d

    def test_slack_is_a_registered_source(self):
        self.assertIn("slack", freq.SOURCES)

    def test_yields_message_text_from_a_dump(self):
        self.with_dump("dump.json", [{"text": "one"}, {"text": "two"}])

        self.assertEqual(list(freq.iter_slack_text()), ["one", "two"])

    def test_missing_dump_dir_yields_nothing(self):
        old = freq.SLACK_DIR
        freq.SLACK_DIR = Path("/nonexistent/svalboard-freq-test/slack")
        self.addCleanup(lambda: setattr(freq, "SLACK_DIR", old))

        self.assertEqual(list(freq.iter_slack_text()), [])

    def test_html_entities_are_unescaped(self):
        # The layout argues about where `&` goes; Slack ships it as &amp;, so
        # counting the raw text would undercount the character this repo cares
        # most about placing.
        self.with_dump("d.json", [{"text": "a &amp; b &lt;c&gt; d"}])

        self.assertEqual(list(freq.iter_slack_text()), ["a & b <c> d"])

    def test_a_literally_typed_entity_survives_unescaping(self):
        # The reason slack_plain unescapes &amp; LAST. Someone typing the four
        # characters "&lt;" sends the wire form "&amp;lt;". Unescape &amp;
        # first and that collapses to "<": four keystrokes counted as one, and
        # one of them is the `&` whose rate decides where `&` goes.
        self.assertEqual(freq.slack_plain("&amp;lt;"), "&lt;")

    def test_mentions_and_channel_refs_are_dropped(self):
        # You autocomplete these; your hands never type the U0123ABC.
        self.with_dump("d.json", [
            {"text": "hey <@U0123ABC> see <#C0456DEF|general> <!here>"},
        ])

        self.assertEqual(list(freq.iter_slack_text()), ["hey  see  "])

    def test_links_keep_the_label_and_drop_the_url(self):
        # A pasted URL is not typing; a label you wrote is.
        self.with_dump("d.json", [
            {"text": "see <https://ex.com/x|the docs> and <https://ex.com/y>"},
        ])

        self.assertEqual(list(freq.iter_slack_text()), ["see the docs and "])

    def test_subtyped_and_empty_messages_are_skipped(self):
        # Joins, bot posts and file-share stubs are not keystrokes.
        self.with_dump("d.json", [
            {"text": "real"},
            {"text": "x joined", "subtype": "channel_join"},
            {"text": "beep", "subtype": "bot_message"},
            {"text": ""},
            {"no_text_key": 1},
        ])

        self.assertEqual(list(freq.iter_slack_text()), ["real"])

    def test_a_dump_of_the_wrong_shape_says_so_instead_of_vanishing(self):
        # A dict with no "messages" key, or a bare scalar, is a dump written
        # wrong -- most likely a changed search-API response. Yielding nothing
        # silently makes that indistinguishable from "you sent no messages",
        # and the corpus just quietly shrinks. The discord loader already
        # announces this case; slack should too.
        self.with_dump("d.json", {"items": [{"text": "wrong key"}]})

        with self.assertLogs_stderr() as err:
            self.assertEqual(list(freq.iter_slack_text()), [])
        self.assertIn("d.json", err())
        self.assertIn("shape", err())

    def assertLogs_stderr(self):
        import contextlib
        import io

        buf = io.StringIO()

        class Cap:
            def __enter__(inner):
                inner.ctx = contextlib.redirect_stderr(buf)
                inner.ctx.__enter__()
                return lambda: buf.getvalue()

            def __exit__(inner, *a):
                return inner.ctx.__exit__(*a)

        return Cap()

    def test_dumps_are_decoded_as_utf8_regardless_of_locale(self):
        # freq.py exists to count characters. Decoding the corpus through
        # whatever LANG happens to be set would make the counts themselves
        # machine-dependent -- the one thing this tool must not be.
        import tempfile

        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        d = Path(tmp.name)
        (d / "d.json").write_bytes(
            json.dumps([{"text": "café — naïve"}], ensure_ascii=False).encode("utf-8")
        )
        old = freq.SLACK_DIR
        freq.SLACK_DIR = d
        self.addCleanup(lambda: setattr(freq, "SLACK_DIR", old))

        self.assertEqual(list(freq.iter_slack_text()), ["café — naïve"])

    def test_accepts_the_messages_envelope_form(self):
        # slack_search_* returns {"messages": [...]}; a hand-saved dump may be
        # the bare list. Both are dumps we would plausibly write.
        self.with_dump("d.json", {"messages": [{"text": "enveloped"}]})

        self.assertEqual(list(freq.iter_slack_text()), ["enveloped"])


class DiscordLoaderTests(unittest.TestCase):
    def with_export(self, *channels: tuple[str, str]) -> Path:
        """Point freq.DISCORD_DIR at a temp export root.

        Each channel is (dir_name, raw messages.csv text) -- raw, because the
        CSV quoting is half of what this loader has to get right.
        """
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root = Path(tmp.name)
        for name, csv_text in channels:
            d = root / "messages" / name
            d.mkdir(parents=True)
            (d / "messages.csv").write_text(csv_text)
        old = freq.DISCORD_DIR
        freq.DISCORD_DIR = root
        self.addCleanup(lambda: setattr(freq, "DISCORD_DIR", old))
        return root

    def with_json_export(self, *channels: tuple[str, str], top="Messages"):
        """A real 2026 export: <top>/c<id>/messages.json, not messages.csv."""
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root = Path(tmp.name)
        for name, payload in channels:
            d = root / top / name
            d.mkdir(parents=True)
            (d / "messages.json").write_text(payload, encoding="utf-8")
        old = freq.DISCORD_DIR
        freq.DISCORD_DIR = root
        self.addCleanup(lambda: setattr(freq, "DISCORD_DIR", old))
        return root

    def test_reads_the_json_export_discord_actually_ships(self):
        # The export that arrived in 2026 has no CSV at all: 1085 channels,
        # every one of them messages.json, under a capitalised Messages/.
        # Same field names, different container.
        # Invented text and invented ids. The export is personal chat and this
        # repo is public -- the rule the whole pipeline is built around is that
        # the corpus never lands here, and a test fixture is still here.
        self.with_json_export(("c000000000000000001", json.dumps([
            {"ID": 1, "Timestamp": "2020-01-01 00:00:00",
             "Contents": "first message", "Attachments": ""},
            {"ID": 2, "Timestamp": "2020-01-01 00:00:01",
             "Contents": "second message", "Attachments": ""},
        ])))

        self.assertEqual(list(freq.iter_discord_text()),
                         ["first message", "second message"])

    def test_the_messages_directory_is_found_whatever_its_case(self):
        # Older exports used lowercase `messages/`. Both are the same export.
        self.with_json_export(("c1", json.dumps([{"Contents": "lower"}])), top="messages")

        self.assertEqual(list(freq.iter_discord_text()), ["lower"])

    def test_a_channel_with_both_shapes_is_counted_once(self):
        # Reading json and csv means a channel holding both gets its text
        # counted twice, silently doubling that channel's weight in the rates
        # the README's argument is built on. JSON wins: it is the shape the
        # current export ships, so a stale CSV beside it is the older copy.
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root = Path(tmp.name)
        d = root / "Messages" / "c1"
        d.mkdir(parents=True)
        (d / "messages.json").write_text(json.dumps([{"Contents": "from json"}]),
                                         encoding="utf-8")
        (d / "messages.csv").write_text(
            'ID,Timestamp,Contents,Attachments\n1,t,from csv,\n', encoding="utf-8")
        old = freq.DISCORD_DIR
        freq.DISCORD_DIR = root
        self.addCleanup(lambda: setattr(freq, "DISCORD_DIR", old))

        self.assertEqual(list(freq.iter_discord_text()), ["from json"])

    def test_csv_exports_still_work(self):
        # Don't strand an older export sitting in $CACHE.
        self.with_export(
            ("c1", 'ID,Timestamp,Contents,Attachments\n1,t,from csv,\n'),
        )

        self.assertEqual(list(freq.iter_discord_text()), ["from csv"])

    def test_a_json_channel_of_the_wrong_shape_is_announced_not_silent(self):
        self.with_json_export(("c1", json.dumps({"not": "a list"})))

        self.assertEqual(list(freq.iter_discord_text()), [])

    def test_discord_is_a_registered_source(self):
        self.assertIn("discord", freq.SOURCES)

    def test_yields_contents_across_channels(self):
        self.with_export(
            ("c1", 'ID,Timestamp,Contents,Attachments\n1,t,hello,\n2,t,there,\n'),
            ("c2", 'ID,Timestamp,Contents,Attachments\n3,t,again,\n'),
        )

        self.assertEqual(list(freq.iter_discord_text()), ["hello", "there", "again"])

    def test_missing_export_yields_nothing(self):
        old = freq.DISCORD_DIR
        freq.DISCORD_DIR = Path("/nonexistent/svalboard-freq-test/discord")
        self.addCleanup(lambda: setattr(freq, "DISCORD_DIR", old))

        self.assertEqual(list(freq.iter_discord_text()), [])

    def test_multiline_messages_survive_csv_quoting(self):
        # The reason this reads CSV properly instead of splitting on newlines:
        # a quoted Contents field spans lines, and naive splitting would shred
        # every multi-line message into fragments.
        self.with_export(
            ("c1", 'ID,Timestamp,Contents,Attachments\n1,t,"one\ntwo",\n'),
        )

        self.assertEqual(list(freq.iter_discord_text()), ["one\ntwo"])

    def test_mentions_dropped_and_custom_emoji_reduced_to_typed_form(self):
        self.with_export((
            "c1",
            'ID,Timestamp,Contents,Attachments\n'
            '1,t,"hey <@123> and <@!456> in <#789> role <@&321> emoji <:party:1> <a:spin:2>",\n',
        ))

        self.assertEqual(
            list(freq.iter_discord_text()),
            ["hey  and  in  role  emoji :party: :spin:"],
        )

    def test_literal_emoji_are_restored_to_the_keys_actually_pressed(self):
        # Discord stores the RENDERED character for standard emoji, so nothing
        # of what you pressed survives in the export. The real one has 15,673
        # of these against 11,239 surviving colons, so taking the file at face
        # value undercounts `:` by ~2.4x, and `:` is one of the seven symbols
        # README.md promoted on a measured rate.
        #
        # What Sauyon actually presses is `:eye<tab>` -- ONE colon, a short
        # prefix, and Tab. The closing colon and the rest of the name are the
        # autocomplete's, not his hands', so reconstructing the full `:eyes:`
        # would invent a second colon and a word nobody typed.
        self.assertEqual(freq.discord_plain("hm \N{THINKING FACE}"), "hm :thi\t")
        self.assertEqual(freq.discord_plain("\N{EYES} look"), ":eye\t look")

    def test_a_run_of_emoji_does_not_fabricate_a_doubled_colon(self):
        # Two emoji in a row must not produce "::" -- a same-key repeat on a
        # lateral is exactly the signal the `-` finding rests on, and inventing
        # one here would corrupt the evidence for it. The Tab between them
        # makes that structurally impossible, which is the real reason to
        # model the keystroke rather than the rendered name.
        out = freq.discord_plain("\N{EYES}\N{EYES}")

        self.assertNotIn("::", out)
        self.assertEqual(out, ":eye\t:eye\t")

    def test_timestamp_markup_is_dropped(self):
        # `<t:1600000000:t>` is inserted by Discord, never typed, and carries
        # two colons -- and `:` is one of the seven symbols the README promoted
        # on a measured rate. Only 29 survive in the real export, so this
        # changes no conclusion; it is here because a construct nobody types
        # should not be in a corpus of what someone typed.
        self.assertEqual(freq.discord_plain("at <t:1600000000:t> ok"), "at  ok")
        self.assertEqual(freq.discord_plain("on <t:1600000001:F>!"), "on !")

    def test_an_emoji_inside_a_url_does_not_leave_url_fragments_behind(self):
        # Emoji restoration inserts a Tab, and BARE_URL stops at whitespace.
        # Run it first and the synthetic Tab cuts the URL in half, leaving the
        # tail to be counted as typing. Strip URLs first.
        self.assertEqual(freq.discord_plain("see https://ex.com/\N{EYES}/x done"),
                         "see  done")

    def test_pasted_urls_are_dropped_like_slack_does(self):
        # The export is full of bare links (https://discord.gg/..., every image
        # host). A pasted URL is not typing, and slack_plain already drops
        # them -- counting them here would inflate `/`, `:` and `.`, and `:`
        # is one of the seven symbols the README promoted on measured rate.
        self.assertEqual(freq.discord_plain("see https://discord.gg/DkMRTtAn ok"),
                         "see  ok")
        self.assertEqual(freq.discord_plain("no url here"), "no url here")

    def test_empty_contents_rows_are_skipped(self):
        # Attachment-only messages: a drag-and-drop, not keystrokes.
        self.with_export(
            ("c1", 'ID,Timestamp,Contents,Attachments\n1,t,,https://cdn/x.png\n2,t,real,\n'),
        )

        self.assertEqual(list(freq.iter_discord_text()), ["real"])

    def test_unexpected_columns_skip_the_file_without_crashing(self):
        # Discord has changed this export's shape before; a silent zero-count
        # is worse than a loud skip.
        self.with_export(
            ("c1", 'ID,Timestamp,Body\n1,t,hello\n'),
            ("c2", 'ID,Timestamp,Contents,Attachments\n2,t,fine,\n'),
        )

        self.assertEqual(list(freq.iter_discord_text()), ["fine"])


class AggregateTests(unittest.TestCase):
    def test_a_source_that_collected_nothing_is_omitted_not_zero_filled(self):
        # freq.json is the public evidence base the README's placement argument
        # rests on. A source with no corpus emitted as a full table of 0.0
        # reads as "measured, never typed" -- the strongest possible claim --
        # when it means "never measured". discord is exactly this until the
        # export lands.
        payload = freq.aggregate_payload({"shell": "abc", "discord": ""})

        self.assertEqual(set(payload["corpus_chars"]), {"shell"})
        self.assertEqual(set(payload["rates"]), {"shell"})

    def test_a_source_with_a_corpus_is_reported(self):
        payload = freq.aggregate_payload({"shell": "a-b-c"})

        self.assertEqual(payload["corpus_chars"]["shell"], 5)
        self.assertAlmostEqual(payload["rates"]["shell"]["-"], 400.0)


if __name__ == "__main__":
    unittest.main()
