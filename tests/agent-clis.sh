#!/usr/bin/env bash
# Cases for which coding-agent CLIs land on which hosts, and for the agent
# drovr's review panel runs on.
#
#   * codex is on every host (programs.codex), with a writable config: codex
#     records project trust in ~/.codex/config.toml itself.
#   * cursor-agent is on utsuho only -- the package, the pool-worker unit and
#     the ~/.cursor tree. Dropping the gate puts it back everywhere, silently,
#     so every host is checked, not one on each side of it.
#   * agy and pi are gone, along with what only they used (the agy-review
#     skill, pi's herdr extension). A leftover home.file pointing at either
#     would leave a skill or hook calling a binary that is no longer installed.
#   * drovr reviews on omp, through an [agents.omp] entry. drovr has no omp
#     built-in, so review_agent = "omp" without the table fails at launch.
#     The file is PARSED (builtins.fromTOML) on every host, not grepped: an
#     unquoted serve_host once made it unloadable on exactly the two hosts
#     that set one, and a grep reads past that. What the reviewer's stance
#     actually keeps out is tests/omp-reviewer-stance.sh's job.
#
# Evaluation only (--max-jobs 0): this never builds a derivation. An optional
# first argument is the flake ref to test (default: this checkout), so the same
# cases can be run against an older commit:
#
#   ./tests/agent-clis.sh
#   ./tests/agent-clis.sh "git+file://$PWD?rev=$(git rev-parse HEAD~1)"
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
flake="${1:-$repo}"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

ev() { # ev <host> <attr-path> [apply] -- raw eval under config.
  nix eval --max-jobs 0 --raw "$flake#homeConfigurations.$1.config.$2" ${3:+--apply "$3"}
}
names() { # names <host> -- space-separated pnames of home.packages
  ev "$1" home.packages 'ps: builtins.concatStringsSep " " (map (p: p.pname or p.name or "") ps)'
}
files() { # files <host> -- newline-separated home.file targets
  ev "$1" home.file 'fs: builtins.concatStringsSep "\n" (map (f: f.target) (builtins.attrValues fs))'
}

hosts="utsuho fujiwara shiori chiyari kyuusaku mari"

for host in $hosts; do
  have=$(names "$host") || { echo "could not evaluate $host" >&2; exit 1; }
  fl=$(files "$host") || { echo "could not evaluate $host home.file" >&2; exit 1; }

  case " $have " in *" codex "*) ok "$host has codex" ;; *) bad "$host is missing codex" ;; esac

  leaked=""
  for p in antigravity-cli pi-coding-agent; do
    case " $have " in *" $p "*) leaked="$leaked $p" ;; esac
  done
  if printf '%s\n' "$fl" | grep -qE '^\.(pi/|claude/(skills|commands)/agy-review)'; then
    leaked="$leaked <agy/pi home.file>"
  fi
  if [ -z "$leaked" ]; then ok "$host carries no agy or pi"
  else bad "$host still carries:$leaked"; fi

  cursor_pkg=no; case " $have " in *" cursor-agent-cli "*) cursor_pkg=yes ;; esac
  cursor_files=no; printf '%s\n' "$fl" | grep -q '^\.cursor/' && cursor_files=yes
  cursor_unit=$(ev "$host" systemd.user.services 's: if s ? "cursor-agent-worker@" then "yes" else "no"')
  want=no; [ "$host" = utsuho ] && want=yes
  if [ "$cursor_pkg/$cursor_files/$cursor_unit" = "$want/$want/$want" ]; then
    ok "$host cursor-agent (package/~/.cursor/unit): $want"
  else
    bad "$host cursor-agent package/~/.cursor/unit: want $want/$want/$want, got $cursor_pkg/$cursor_files/$cursor_unit"
  fi
done

mut=$(ev shiori programs.codex.mutableSettings 'b: if b then "true" else "false"' 2>/dev/null)
if [ "$mut" = true ]; then ok "codex config is writable (mutableSettings)"
else bad "codex mutableSettings: want true, got ${mut:-<unset>}"; fi

# One line per host: what the parsed file says, or "unparseable". The text
# references the overlay's store path, which fromTOML refuses; the context is
# what it objects to, not the TOML, so it is dropped first.
probe='f: let c = builtins.fromTOML (builtins.unsafeDiscardStringContext f."drovr/config.toml".text); o = c.agents.omp or {}; in
  "${c.review_agent or "-"} ${o.mcp.mechanism or "-"} ${o.mcp.flag or "-"} "
  + (if builtins.match ".*--tools [^ ]*submit_findings.*--config /nix/store/.*" (o.readonly_flag or "") != null then "stance" else "-")
  + " " + builtins.concatStringsSep "," (o.readonly_displace or [])'
# readonly_displace is pinned exactly: drovr renames each entry aside and never
# puts it back, so a commonly tracked path here (.claude, .cursor, .env) would
# dirty every reviewed tree. home.nix says why the list stops where it does.
want_drovr="omp generated-module --extension stance .omp,.gemini,.codex,.opencode"
for host in $hosts; do
  got=$(ev "$host" xdg.configFile "$probe" 2>/dev/null) || got=unparseable
  if [ "$got" = "$want_drovr" ]; then
    ok "$host drovr config parses; reviews on omp with the hardened stance"
  else
    bad "$host drovr config: want '$want_drovr', got '$got'"
  fi
done

[ "$fails" -eq 0 ]
