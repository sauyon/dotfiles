#!/usr/bin/env bash
# Cases for the Gas City toolchain (gc + bd + dolt) landing on fujiwara only.
#
# Gas City is a tmux fleet of Claude sessions that bills while it runs, so it
# lives on the one headless box meant to host it. Two ways this breaks quietly:
#
#   * fujiwara loses a piece. gc shells out to `bd` for every bead and runs a
#     managed `dolt` for the city, so gc without either evaluates fine and then
#     dies at `gc init`. Case 1 checks all three are in the closure.
#
#   * the gate widens. Dropping the hostname guard puts gc on every host, and
#     nothing errors. Case 2 checks a non-fujiwara host carries none of them.
#
# Evaluation only (--max-jobs 0): this never builds a derivation.
#
#   ./tests/gascity-packages.sh
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

names() { # names <host> -- space-separated pnames of home.packages
  nix eval --max-jobs 0 --raw "$repo#homeConfigurations.$1.config.home.packages" \
    --apply 'ps: builtins.concatStringsSep " " (map (p: p.pname or p.name or "") ps)'
}

fuji=$(names fujiwara) || { echo "could not evaluate fujiwara" >&2; exit 1; }
missing=""
for p in gascity beads dolt; do
  case " $fuji " in *" $p "*) ;; *) missing="$missing $p" ;; esac
done
if [ -z "$missing" ]; then ok "fujiwara has gascity, beads, dolt"
else bad "fujiwara is missing:$missing"; fi

other=$(names shiori) || { echo "could not evaluate shiori" >&2; exit 1; }
leaked=""
for p in gascity beads dolt; do
  case " $other " in *" $p "*) leaked="$leaked $p" ;; esac
done
if [ -z "$leaked" ] && [ -n "$other" ]; then ok "shiori carries none of them"
else bad "shiori carries:${leaked:- <nothing evaluated>}"; fi

# Case 3: gc's state (city registry, supervisor config, logs) defaults to ~/.gc;
# GC_HOME moves it under XDG state. It has to be a session variable, not just a
# shell one: `gc start` bakes GC_HOME into the supervisor's systemd unit, so a
# shell that lacks it registers the city under ~/.gc instead.
state=$(nix eval --max-jobs 0 --raw "$repo#homeConfigurations.fujiwara.config.xdg.stateHome") \
  || { echo "could not evaluate fujiwara xdg.stateHome" >&2; exit 1; }
gchome=$(nix eval --max-jobs 0 --raw "$repo#homeConfigurations.fujiwara.config.home.sessionVariables.GC_HOME" 2>/dev/null)
if [ -n "$gchome" ] && [ "$gchome" = "$state/gc" ]; then ok "fujiwara GC_HOME is \$XDG_STATE_HOME/gc"
else bad "fujiwara GC_HOME: want $state/gc, got ${gchome:-<unset>}"; fi

[ "$fails" -eq 0 ]
