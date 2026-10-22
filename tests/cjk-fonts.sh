#!/usr/bin/env bash
# Cases for Japanese (and other CJK) text rendering on every host.
#
# Nothing errors when this is missing: fontconfig falls back to Noto Sans,
# which has no kana or kanji, and the text renders as tofu. It went missing
# twice without anyone noticing -- trimming google-fonts to a named list dropped
# Noto Sans JP, and dropping bootstrap.sh dropped the `pacman -S noto-fonts-cjk`
# the Arch hosts used to get -- so the claim is checked here rather than by eye.
#
# Two claims, each failing silently on its own:
#
#   * the CJK font package is in every host's home.packages (hosts are read out
#     of the flake, so a new host is covered the day it lands);
#   * the realised package's fonts actually declare `ja` coverage to fontconfig,
#     checked with fc-scan on hosts of this machine's own system.
#
#   ./tests/cjk-fonts.sh            # tests this checkout
#   ./tests/cjk-fonts.sh /path/to/flake
set -u
set -o pipefail

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

PKG=noto-fonts-cjk-sans
LANGS="ja ko zh-cn zh-tw"

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }

command -v fc-scan >/dev/null || { echo "fc-scan not on PATH" >&2; exit 1; }

here=$(nix eval --raw --impure --expr builtins.currentSystem) \
  || { echo "could not ask nix for the current system" >&2; exit 1; }

# --max-jobs 0: this is an eval, and must stay one (see tests/hmeval.sh).
systems=$(nix eval --max-jobs 0 --json "$FLAKE#homeConfigurations" \
  --apply 'hs: builtins.mapAttrs (_: h: h.pkgs.stdenv.hostPlatform.system) hs' 2>/dev/null) \
  || { echo "could not list homeConfigurations" >&2; exit 1; }
hosts=$(jq -r 'keys[]' <<<"$systems")
[ -n "$hosts" ] || { echo "this flake has no homeConfigurations" >&2; exit 1; }

checked=""
for h in $hosts; do
  sys=$(jq -r --arg h "$h" '.[$h]' <<<"$systems")
  out=$(nix eval --max-jobs 0 --raw "$FLAKE#homeConfigurations.$h.config.home.packages" \
    --apply "ps: let m = builtins.filter (p: (p.pname or \"\") == \"$PKG\") ps;
             in if m == [] then \"\" else (builtins.head m).outPath" 2>/dev/null) \
    || { fail "$h" "home.packages did not evaluate"; continue; }
  if [ -z "$out" ]; then
    fail "$h" "$PKG is not in home.packages"
    continue
  fi
  pass "$h: $PKG in home.packages"

  [ "$sys" = "$here" ] || continue
  case " $checked " in *" $out "*) continue ;; esac
  checked="$checked $out"
  nix-store --realise "$out" >/dev/null 2>&1 \
    || { fail "$h" "could not realise $out"; continue; }
  # Every face's lang set, one per line; a language counts if any face has it.
  covered=$(find "$out/share/fonts" \( -name '*.otf' -o -name '*.ttc' -o -name '*.ttf' \) \
    -exec fc-scan --format '%{lang}\n' {} + 2>/dev/null | tr '|' '\n' | sort -u)
  for l in $LANGS; do
    if grep -qx -- "$l" <<<"$covered"; then
      pass "$h: fontconfig sees $l coverage"
    else
      fail "$h: $l" "no face in $out declares $l"
    fi
  done
done

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
