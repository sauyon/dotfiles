#!/usr/bin/env python3
"""Measure how often characters actually get typed, for keymap decisions.

The layout's symbol placements are argued from measured frequency rather than
feel (see README). This builds the corpus that argument rests on, caches it, and
reports per-1000-character rates -- so the next "should I promote X?" is one
command instead of a re-derivation.

    python3 freq.py                # refresh corpus, report the promoted set
    python3 freq.py '`' '~'        # rates for specific characters
    python3 freq.py --all          # every printable ASCII symbol
    python3 freq.py --no-refresh   # report from the cache, don't re-read sources

WHERE THE CORPUS LIVES, AND WHY NOT HERE
----------------------------------------
The cache goes in ~/.local/share/svalboard-freq/, never in this repo. This repo
is public; the corpus is shell history and prompts, which carry internal
hostnames, paths and work detail. What IS safe to commit is the aggregate --
per-1000 rates for single characters reveal nothing about content -- so freq.py
writes freq.json here and that is what the README's table is built from.

If you ever change the extraction, delete the cache rather than trusting it:
the manifest records the filters that produced it, but not their intent.

THE CORPORA
-----------
`prompts`  Text you typed to agents. Sidechains (subagent transcripts) and
           machine-injected blocks are dropped -- see EXCLUDE_PREFIXES. What
           survives is meant to be keystrokes your hands actually made, which
           is the thing a keymap should be optimised for.
`shell`    zsh history, with the `: <ts>:<elapsed>;` metadata stripped so the
           timestamps don't dilute the rates.
`code`     Text files tracked in this repo, minus generated artifacts. Counting
           a generated .vil or the rendered cheatsheet would be measuring this
           script's own output, not typing.
`slack`    Work chat. Not fetched by this script -- freq.py has no network and
           no Slack token. Drop JSON dumps into $CACHE/slack/ (see
           iter_slack_text) and this reads them.
`discord`  Personal chat, from the official data export -- not an API scrape,
           which would be a self-bot and bannable. See iter_discord_text.

A rate is per 1000 characters of the corpus, so the corpora stay comparable
despite very different sizes.
"""

import argparse
import csv
import json
import pathlib
import re
import subprocess
import sys
from collections import Counter
from datetime import datetime, timezone

HERE = pathlib.Path(__file__).parent
REPO = HERE.parent.parent
CACHE = pathlib.Path.home() / ".local/share/svalboard-freq"
CORPUS_DIR = CACHE / "corpus"
SLACK_DIR = CACHE / "slack"
DISCORD_DIR = CACHE / "discord"
MANIFEST = CACHE / "manifest.json"
AGGREGATE = HERE / "freq.json"

PROJECTS = pathlib.Path.home() / ".config/claude-work/projects"
ZSH_HISTORY = pathlib.Path.home() / ".local/share/zsh/history"

# The symbols the README argues about, in the order that table lists them.
PROMOTED = ["-", ":", "(", ")", "&"]

# Blocks that arrive in a `user` turn without anyone typing them: hook output,
# tool results rendered as text, slash-command scaffolding, system nudges. They
# are the bulk of a long transcript and would swamp the real keystrokes.
EXCLUDE_PREFIXES = (
    "<system-reminder>", "<command-name>", "<command-message>", "<command-args>",
    "<local-command-stdout>", "<local-command-stderr>", "<task-notification>",
    "<user-prompt-submit-hook>", "<session-start-hook>", "<bash-input>",
    "<bash-stdout>", "<bash-stderr>", "Caveat: The messages below",
    "[Request interrupted", "API Error", "<EXTREMELY_IMPORTANT>",
    "DROVR GATE", "<SUBAGENT-STOP>",
)

# Generated or vendored: counting these measures a generator, not a person.
CODE_SKIP = {
    "flake.lock", "SvalHandsDownNeu.vil", "cheatsheet.html", "freq.json",
    "opencode.json", "secrets.yaml",
}
CODE_SKIP_SUFFIX = {".lock", ".png", ".jpg", ".jpeg", ".pdf", ".zst", ".gz", ".vil"}


def iter_prompt_text():
    """Hand-typed user text from agent transcripts.

    Yields one string per surviving block. `find` does not work under this
    home (systemd-homed), so the transcripts are globbed.
    """
    files = sorted(PROJECTS.glob("*/*.jsonl"))
    kept = dropped = 0
    for path in files:
        try:
            raw = path.read_text(errors="replace")
        except OSError:
            continue
        for line in raw.splitlines():
            if not line.strip():
                continue
            try:
                rec = json.loads(line)
            except (json.JSONDecodeError, ValueError):
                continue
            if rec.get("isSidechain"):
                continue
            if rec.get("type") != "user":
                continue
            msg = rec.get("message")
            if not isinstance(msg, dict) or msg.get("role") != "user":
                continue

            content = msg.get("content")
            blocks = []
            if isinstance(content, str):
                blocks = [content]
            elif isinstance(content, list):
                for b in content:
                    # tool_result blocks are output, not typing.
                    if isinstance(b, dict) and b.get("type") == "text":
                        t = b.get("text")
                        if isinstance(t, str):
                            blocks.append(t)

            for text in blocks:
                stripped = text.lstrip()
                if any(stripped.startswith(p) for p in EXCLUDE_PREFIXES):
                    dropped += 1
                    continue
                kept += 1
                yield text
    print(f"  prompts: {len(files)} transcripts, {kept} blocks kept, "
          f"{dropped} machine-injected blocks dropped", file=sys.stderr)


# zsh extended history: ": <epoch>:<elapsed>;<command>", commands may continue
# onto following lines with a trailing backslash.
ZSH_META = re.compile(r"^: \d+:\d+;")


def iter_shell_text():
    if not ZSH_HISTORY.exists():
        print(f"  shell: {ZSH_HISTORY} missing, skipping", file=sys.stderr)
        return
    raw = ZSH_HISTORY.read_text(errors="replace")
    n = 0
    for line in raw.splitlines():
        cmd = ZSH_META.sub("", line)
        if not cmd.strip():
            continue
        n += 1
        yield cmd
    print(f"  shell: {n} history lines", file=sys.stderr)


def iter_code_text():
    """Text files tracked in this repo, minus generated artifacts."""
    try:
        out = subprocess.run(
            ["git", "-C", str(REPO), "ls-files", "-z"],
            capture_output=True, check=True,
        ).stdout.decode(errors="replace")
    except (subprocess.CalledProcessError, FileNotFoundError) as exc:
        print(f"  code: git ls-files failed ({exc}), skipping", file=sys.stderr)
        return

    n = 0
    for rel in out.split("\0"):
        if not rel:
            continue
        p = pathlib.Path(rel)
        if p.name in CODE_SKIP or p.suffix in CODE_SKIP_SUFFIX:
            continue
        full = REPO / p
        try:
            text = full.read_text(errors="strict")
        except (OSError, UnicodeDecodeError):
            continue  # binary or unreadable
        n += 1
        yield f"\x00PATH:{rel}\n{text}"
    print(f"  code: {n} tracked text files", file=sys.stderr)


# Slack's own markup, none of which anyone types by hand:
#   <@U0123ABC>  <@U0123ABC|name>       a mention, autocompleted from a picker
#   <#C0456DEF|general>                 a channel ref, same
#   <!here> <!channel> <!subteam^S1|@x> a broadcast, same
# and links, which are `<url>` bare or `<url|label>` when you gave them text.
SLACK_ENTITY = re.compile(r"<[@#!][^<>]*>")
SLACK_LINK_LABELLED = re.compile(r"<[^<>|]+\|([^<>]*)>")
SLACK_LINK_BARE = re.compile(r"<[^<>|]+>")


def slack_plain(text):
    """Slack's wire format back to the characters a person actually pressed.

    Two things happen here and the second one is the point. Entities and bare
    URLs come out: a mention is a click in a picker and a pasted URL is a
    paste, so counting either would credit the keyboard for keys nobody hit.
    Labels survive, because you typed those.

    Then `&amp;` `&lt;` `&gt;` are unescaped. That matters more than it looks:
    this layout's whole argument for where `&` goes is a measured rate, and
    Slack ships every typed `&` as `&amp;` -- reading the raw text would count
    an `a`, an `m` and a `p` instead, and undercount the one character the
    README spends a paragraph placing. Unescape `&amp;` last so a literal
    "&amp;lt;" resolves to "&lt;" rather than to "<".
    """
    text = SLACK_ENTITY.sub("", text)
    text = SLACK_LINK_LABELLED.sub(r"\1", text)
    text = SLACK_LINK_BARE.sub("", text)
    return text.replace("&lt;", "<").replace("&gt;", ">").replace("&amp;", "&")


def iter_slack_text():
    """Messages you sent on Slack, from JSON dumps in $CACHE/slack/.

    freq.py does not fetch these. It has no network and no Slack token, and
    keeping it that way is deliberate -- the fetch needs credentials and a
    `from:@me` scope decision, and neither belongs in a frequency counter. Put
    dumps in $CACHE/slack/ yourself: either a bare JSON list of message objects
    or the `{"messages": [...]}` envelope the search API returns. Anything with
    a `subtype` is a join, a bot post or a file-share stub rather than
    something you typed, so it is skipped.

    Like the rest of the corpus these stay in $CACHE and are never committed --
    work Slack is the single most sensitive source here.
    """
    if not SLACK_DIR.is_dir():
        print(f"  slack: {SLACK_DIR} missing, skipping", file=sys.stderr)
        return
    files = sorted(SLACK_DIR.glob("*.json"))
    n = skipped = 0
    for path in files:
        try:
            payload = json.loads(path.read_text(errors="replace"))
        except (OSError, json.JSONDecodeError, ValueError) as exc:
            print(f"  slack: {path.name} unreadable ({exc}), skipping", file=sys.stderr)
            continue
        if isinstance(payload, dict):
            payload = payload.get("messages", [])
        if not isinstance(payload, list):
            continue
        for msg in payload:
            if not isinstance(msg, dict):
                continue
            if msg.get("subtype"):
                skipped += 1
                continue
            text = msg.get("text")
            if not isinstance(text, str) or not text:
                skipped += 1
                continue
            n += 1
            yield slack_plain(text)
    print(f"  slack: {len(files)} dumps, {n} messages, {skipped} non-typed skipped",
          file=sys.stderr)


# Discord's markup. Mentions and channel/role refs come out of a picker;
# custom emoji are stored as <:name:id> but typed as `:name:`, so they reduce
# to the form your hands actually made rather than vanishing.
DISCORD_MENTION = re.compile(r"<[@#][!&]?\d+>")
DISCORD_EMOJI = re.compile(r"<a?:(\w+):\d+>")


def discord_plain(text):
    return DISCORD_EMOJI.sub(r":\1:", DISCORD_MENTION.sub("", text))


def iter_discord_text():
    """Messages you sent on Discord, from the official data export.

    NOT an API scraper, on purpose. Discord offers no ToS-clean way to read
    your own history programmatically: a user token against the API is a
    self-bot and is bannable, and a bot user can only see guilds you administer
    and never your DMs. The clean route is the export you are entitled to --
    Settings -> Data & Privacy -> Request all my data -- which takes days to a
    month to arrive. Unzip it to $CACHE/discord/ so that
    $CACHE/discord/messages/<channel>/messages.csv exists.

    Everything in that export is yours by construction, so there is no author
    filtering to do. Read with the csv module rather than by lines: Contents is
    a quoted field and multi-line messages span lines inside it.
    """
    root = DISCORD_DIR / "messages"
    if not root.is_dir():
        print(f"  discord: {root} missing, skipping "
              "(request the export: Settings -> Data & Privacy)", file=sys.stderr)
        return
    paths = sorted(root.glob("*/messages.csv"))
    n = skipped = 0
    for path in paths:
        try:
            with path.open(newline="", errors="replace") as fh:
                reader = csv.DictReader(fh)
                if reader.fieldnames is None or "Contents" not in reader.fieldnames:
                    print(f"  discord: {path.parent.name} has no Contents column "
                          f"({reader.fieldnames}), skipping", file=sys.stderr)
                    continue
                for row in reader:
                    text = row.get("Contents")
                    if not text:
                        skipped += 1
                        continue
                    n += 1
                    yield discord_plain(text)
        except (OSError, csv.Error) as exc:
            print(f"  discord: {path} unreadable ({exc}), skipping", file=sys.stderr)
    print(f"  discord: {len(paths)} channels, {n} messages, "
          f"{skipped} without typed text skipped", file=sys.stderr)


SOURCES = {
    "prompts": iter_prompt_text,
    "shell": iter_shell_text,
    "code": iter_code_text,
    "slack": iter_slack_text,
    "discord": iter_discord_text,
}


def build_corpus():
    CORPUS_DIR.mkdir(parents=True, exist_ok=True)
    manifest = {
        "built": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "sources": {},
        "note": "Raw corpus. Contains shell history and prompts -- never commit.",
    }
    print("building corpus:", file=sys.stderr)
    for name, fn in SOURCES.items():
        parts = list(fn())
        text = "\n".join(parts)
        (CORPUS_DIR / f"{name}.txt").write_text(text)
        manifest["sources"][name] = {"chars": len(text), "blocks": len(parts)}
    MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


def load_corpus():
    if not MANIFEST.exists():
        raise SystemExit(
            "no cached corpus -- run without --no-refresh to build it first"
        )
    out = {}
    for name in SOURCES:
        p = CORPUS_DIR / f"{name}.txt"
        out[name] = p.read_text(errors="replace") if p.exists() else ""
    return out


def rates(corpus, chars):
    """Occurrences per 1000 characters, per corpus."""
    table = {}
    for name, text in corpus.items():
        n = len(text)
        counts = Counter(text)
        table[name] = {
            c: (counts.get(c, 0) * 1000.0 / n) if n else 0.0 for c in chars
        }
        table[name]["_chars"] = n
    return table


def code_split(corpus, char):
    """Per-extension breakdown for the code corpus.

    This is the check that killed `*`: 6.7 per 1000 looked promotable until it
    turned out to be `**` in markdown prose. A symbol that lives in one file
    type is a symbol you type in one context.
    """
    text = corpus.get("code", "")
    by_ext = Counter()
    tot_ext = Counter()
    for chunk in text.split("\x00PATH:"):
        if not chunk:
            continue
        head, _, body = chunk.partition("\n")
        ext = pathlib.Path(head.strip()).suffix or "(none)"
        by_ext[ext] += body.count(char)
        tot_ext[ext] += len(body)
    return by_ext, tot_ext


def fenced_split(corpus, char):
    """For the prompts corpus: inside ``` fences vs outside.

    Backticks especially: a rate driven entirely by fenced code blocks is a
    rate driven by pasting, not by prose typing.
    """
    text = corpus.get("prompts", "")
    inside = outside = 0
    in_fence = False
    for line in text.split("\n"):
        if line.lstrip().startswith("```"):
            in_fence = not in_fence
            inside += line.count(char)
            continue
        if in_fence:
            inside += line.count(char)
        else:
            outside += line.count(char)
    return inside, outside


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("chars", nargs="*", help="characters to report (default: the promoted set)")
    ap.add_argument("--all", action="store_true", help="every printable ASCII symbol")
    ap.add_argument("--no-refresh", action="store_true", help="use the cached corpus")
    args = ap.parse_args()

    if args.no_refresh:
        corpus = load_corpus()
    else:
        build_corpus()
        corpus = load_corpus()

    if args.all:
        chars = [c for c in map(chr, range(33, 127)) if not c.isalnum()]
    elif args.chars:
        chars = []
        for a in args.chars:
            chars.extend(list(a))
    else:
        chars = PROMOTED

    table = rates(corpus, chars)

    print()
    print("per 1000 characters")
    widths = max(6, max((len(repr(c)) for c in chars), default=6))
    hdr = "  " + "corpus".ljust(10) + "".join(repr(c).center(widths + 2) for c in chars)
    print(hdr)
    print("  " + "-" * (len(hdr) - 2))
    for name in SOURCES:
        row = "  " + name.ljust(10)
        for c in chars:
            row += f"{table[name][c]:.2f}".center(widths + 2)
        print(row)
    print()
    for name in SOURCES:
        print(f"  {name}: {table[name]['_chars']:,} chars")

    # Context checks, only worth printing for a handful of characters.
    if len(chars) <= 4:
        for c in chars:
            print(f"\n  --- context for {c!r} ---")
            inside, outside = fenced_split(corpus, c)
            tot = inside + outside
            if tot:
                print(f"  prompts: {outside:,} outside ``` fences, {inside:,} inside "
                      f"({100 * inside / tot:.0f}% fenced)")
            by_ext, tot_ext = code_split(corpus, c)
            top = by_ext.most_common(5)
            if top and sum(by_ext.values()):
                s = sum(by_ext.values())
                for ext, n in top:
                    if not n:
                        continue
                    per1k = n * 1000.0 / tot_ext[ext] if tot_ext[ext] else 0
                    print(f"  code {ext:<8} {n:>6} occurrences "
                          f"({100 * n / s:>4.0f}% of all) {per1k:>6.2f}/1k in that type")

    # The publishable half: aggregate rates only, no corpus.
    every = [c for c in map(chr, range(33, 127)) if not c.isalnum()]
    agg = rates(corpus, every)
    AGGREGATE.write_text(json.dumps({
        "generated_by": "freq.py",
        "note": "Per-1000-character rates. Aggregates only -- the corpus itself "
                "is local (~/.local/share/svalboard-freq) and unpublishable.",
        "corpus_chars": {k: agg[k]["_chars"] for k in SOURCES},
        "rates": {
            k: {c: round(agg[k][c], 4) for c in every} for k in SOURCES
        },
    }, indent=2) + "\n")
    print(f"\nwrote {AGGREGATE.relative_to(REPO)} (aggregates only)")
    print(f"corpus cached in {CACHE} (local, not committed)")


if __name__ == "__main__":
    sys.exit(main())
