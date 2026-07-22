#!/bin/sh
# Names this pane in herdr's Agents panel after Claude's current OSC terminal
# title (its rolling conversation summary). herdr captures that title only for
# working/idle/blocked state detection and never surfaces it as a display name,
# so we read it back and push it as the pane's display_agent via report-metadata.
#
# Companion to the managed herdr-agent-state.sh (which reports session identity
# + state); safe to run alongside it. No-op unless running inside a herdr pane
# (HERDR_ENV=1), so it is inert outside herdr and for non-herdr Claude sessions.
set -eu

[ "${HERDR_ENV:-}" = "1" ] || exit 0
[ -n "${HERDR_PANE_ID:-}" ] || exit 0
command -v herdr >/dev/null 2>&1 || exit 0
command -v python3 >/dev/null 2>&1 || exit 0

# Skip subagent sessions so a Task sub-agent never renames the top-level pane.
input="$(cat 2>/dev/null || true)"
case "$input" in *'"agent_id"'*) exit 0 ;; esac

# Pull the pane's captured OSC title out of herdr's detection state, then strip a
# leading activity/spinner glyph (Claude uses a braille spinner) and its trailing
# whitespace — mirroring herdr's own terminal_title_stripped normalization.
title="$(herdr agent explain "$HERDR_PANE_ID" --json 2>/dev/null | python3 -c '
import sys, json, re
try:
    rules = json.load(sys.stdin).get("evaluated_rules", [])
except Exception:
    raise SystemExit(0)
t = next((r.get("evidence", {}).get("region_preview", "")
          for r in rules if r.get("region") == "osc_title"), "")
print(re.sub(r"^[⠀-⣿]+\s*", "", t).strip())
' 2>/dev/null)"

# Empty title (e.g. an idle pane that cleared it) — leave the last good name in
# place rather than blanking it.
[ -n "$title" ] || exit 0

herdr pane report-metadata "$HERDR_PANE_ID" \
  --source "herdr:claude-name" \
  --display-agent "$title" >/dev/null 2>&1 || true
exit 0
